#include <Arduino.h>

#include <DAC.h>
#include <Scheduler.h>

//#include "Adafruit_TPA2016.h"

//Adafruit_TPA2016 audioamp = Adafruit_TPA2016();


#define NUM_CHANNELS 4
/* ch7:A0 ch6:A1 ch5:A2 ch4:A3 ch3:A4 ch2:A5 ch1:A6 ch0:A7 */
#define ADC_CHANNELS ADC_CHER_CH7 | ADC_CHER_CH5 | ADC_CHER_CH4 | ADC_CHER_CH6
#define BUFFER_SIZE 500*NUM_CHANNELS
#define NUMBER_OF_BUFFERS 5  /// Make this 3 or greater


unsigned int samplingRate = 100000;

class ADCSampler {
  public:
    ADCSampler();
    void begin(unsigned int samplingRate);
    void end();
    void handleInterrupt();
    bool available();
    uint16_t* getFilledBuffer(int *bufferLength);
    void readBufferDone();
  private:
    volatile bool dataReady;
    uint16_t adcBuffer[NUMBER_OF_BUFFERS][BUFFER_SIZE];
    unsigned int adcDMAIndex;        //!< This hold the index of the next DMA buffer
    unsigned int adcTransferIndex;   //!< This hold the last filled buffer

};

ADCSampler::ADCSampler()
{
  dataReady = false;
  adcDMAIndex = 0;
  adcTransferIndex = 0;
  for (int i = 0; i < NUMBER_OF_BUFFERS; i++)
  {
    memset((void *)adcBuffer[i], 0, BUFFER_SIZE);
  }
}
void ADCSampler::begin(unsigned int samplingRate)
{
  // Turning devices Timer on.
  pmc_enable_periph_clk(ID_TC0);


  // Configure timer
  TC_Configure(TC0, 0, TC_CMR_WAVE | TC_CMR_WAVSEL_UP_RC | TC_CMR_ACPA_CLEAR | TC_CMR_ACPC_SET | TC_CMR_ASWTRG_CLEAR | TC_CMR_TCCLKS_TIMER_CLOCK1);

  // It is good to have the timer 0 on PIN2, good for Debugging
  //int result = PIO_Configure( PIOB, PIO_PERIPH_B, PIO_PB25B_TIOA0, PIO_DEFAULT);

  // Configure ADC pin A7
  //  the below code is taken from adc_init(ADC, SystemCoreClock, ADC_FREQ_MAX, ADC_STARTUP_FAST);

  ADC->ADC_CR = ADC_CR_SWRST;         // Reset the controller.
  ADC->ADC_MR = 0;                    // Reset Mode Register.
  ADC->ADC_PTCR = (ADC_PTCR_RXTDIS | ADC_PTCR_TXTDIS); // Reset PDC transfer.

  ADC->ADC_MR |= ADC_MR_PRESCAL(3);   // ADC clock = MSCK/((PRESCAL+1)*2), 13 -> 750000 Sps
  ADC->ADC_MR |= ADC_MR_STARTUP_SUT0; // What is this by the way?
  ADC->ADC_MR |= ADC_MR_TRACKTIM(15);
  ADC->ADC_MR |= ADC_MR_TRANSFER(1);
  ADC->ADC_MR |= ADC_MR_TRGEN_EN;
  ADC->ADC_MR |= ADC_MR_TRGSEL_ADC_TRIG1; // selecting TIOA0 as trigger.
  ADC->ADC_CHER = ADC_CHANNELS;

  /* Interupts */
  ADC->ADC_IDR   = ~ADC_IDR_ENDRX;
  ADC->ADC_IER   =  ADC_IER_ENDRX;
  /* Waiting for ENDRX as end of the transfer is set
    when the current DMA transfer is done (RCR = 0), i.e. it doesn't include the
    next DMA transfer.

    If we trigger on RXBUFF This flag is set if there is no more DMA transfer in
    progress (RCR = RNCR = 0). Hence we may miss samples.
  */

  
  unsigned int cycles = 42000000 / samplingRate;

  /*  timing of ADC */
  TC_SetRC(TC0, 0, cycles);      // TIOA0 goes HIGH on RC.
  TC_SetRA(TC0, 0, cycles / 2);  // TIOA0 goes LOW  on RA.

  // We have to reinitalise just in case the Sampler is stopped and restarted...
  dataReady = false;
  adcDMAIndex = 0;
  adcTransferIndex = 0;
  for (int i = 0; i < NUMBER_OF_BUFFERS; i++)
  {
    memset((void *)adcBuffer[i], 0, BUFFER_SIZE);
  }

  ADC->ADC_RPR  = (unsigned long) adcBuffer[adcDMAIndex];  // DMA buffer
  ADC->ADC_RCR  = (unsigned int)  BUFFER_SIZE;  // ADC works in half-word mode.
  ADC->ADC_RNPR = (unsigned long) adcBuffer[(adcDMAIndex + 1)];  // next DMA buffer
  ADC->ADC_RNCR = (unsigned int)  BUFFER_SIZE;

  // Enable interrupts
  NVIC_EnableIRQ(ADC_IRQn);
  ADC->ADC_PTCR  =  ADC_PTCR_RXTEN;  // Enable receiving data.
  ADC->ADC_CR   |=  ADC_CR_START;    //start waiting for trigger.

  // Start timer
  TC0->TC_CHANNEL[0].TC_SR;
  TC0->TC_CHANNEL[0].TC_CCR = TC_CCR_CLKEN;
  TC_Start(TC0, 0);
}

void ADCSampler::handleInterrupt()
{
  
  unsigned long status = ADC->ADC_ISR;
  if (status & ADC_ISR_ENDRX)  {
    
  //Serial.print("ADC\n");
    adcTransferIndex = adcDMAIndex;
    adcDMAIndex = (adcDMAIndex + 1) % NUMBER_OF_BUFFERS;
    ADC->ADC_RNPR  = (unsigned long) adcBuffer[(adcDMAIndex + 1) % NUMBER_OF_BUFFERS];
    ADC->ADC_RNCR  = BUFFER_SIZE;
    dataReady = true;
  }
}
bool ADCSampler::available()
{
  return dataReady;
}

uint16_t* ADCSampler::getFilledBuffer(int *bufferLength)
{
  *bufferLength = BUFFER_SIZE;
  return adcBuffer[adcTransferIndex];
}

void ADCSampler::readBufferDone()
{
  dataReady = false;
}


ADCSampler sampler;


void setup() {

  Scheduler.startLoop(loopADC);
  sampler.begin(samplingRate);

  //if(sw==LOW){
  //  player.begin(playerSampRate*2, WAV_DATA, WAV_DATA_LEN);
  //}
  //else{
  //player.begin(playerSampRate*2, WAV_DATA, WAV_DATA_LEN);
  //}
  //Serial.print("end");
  //SerialUSB.println("end");
  //SerialUSB.flush();
  //dac.begin(samplingRate, WAV_DATA, WAV_DATA_LEN);
}

int s=0;


  
void loop() {
  yield();
  //player.push();
  //SerialUSB.print(".");
}



void loopADC() {
  //Serial.print("looping\n");
  if (sampler.available()) {
    int bufferLength = 0;
    uint16_t* cBuf = sampler.getFilledBuffer(&bufferLength);
    
    //Serial.print("adc_out:");
    //Serial.print(bufferLength);
    //Serial.print("\n");
    SerialUSB.write((uint8_t *)cBuf,bufferLength*2);
    sampler.readBufferDone();
  }
  yield();
}

void ADC_Handler() {
  sampler.handleInterrupt();
}



//void DACC_Handler(void) {
  //dac.handleInterrupt();
//}


