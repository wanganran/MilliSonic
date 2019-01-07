package v2

//{x,y,z} ratio that, at nearest pos, what's the length
class Translation3DGUI(xratio:Float, yratio:Float, zratio:Float) {
  def translate(pos:(Float, Float, Float))={
    pos match {
      case (x,y,z)=>
        (y/x*xratio/yratio*0.5f,
          z/x*xratio/zratio*0.5f)
    }
  }
  def translateToBox(p:(Float, Float, Float), width:(Float ,Float, Float), qua:(Float, Float, Float, Float))= {
    def q2m() = {
      qua match {
        case (qw, qx, qy, qz) =>
          Array(1 - 2 * qy * qy - 2 * qz * qz, 2 * qx * qy - 2 * qz * qw, 2 * qx * qz + 2 * qy * qw,
            2 * qx * qy + 2 * qz * qw, 1 - 2 * qx * qx - 2 * qz * qz, 2 * qy * qz - 2 * qx * qw,
            2 * qx * qz - 2 * qy * qw, 2 * qy * qz + 2 * qx * qw, 1 - 2 * qx * qx - 2 * qy * qy
          )
      }
    }

    def mult(mat: Array[Float], vec: (Float, Float, Float)) = vec match {
      case (x, y, z) => {
        (
          x * mat(0) + y * mat(1) + z * mat(2),
          x * mat(3) + y * mat(4) + z * mat(5),
          x * mat(6) + y * mat(7) + z * mat(8)
        )
      }
    }

    val mat = q2m()

    val unit = Array(-0.5f, 0.5f)
    val pos = for (x <- unit; y <- unit; z <- unit) yield
      (x, y, z)

    (p, width) match {
      case ((x, y, z), (wx, wy, wz)) =>
        val points=pos.map(mult(mat, _)).map {
          case (dx, dy, dz) =>
            (dx * wx + x, dy * wy + y, dz * wz + z)
        }
        Array(
          (points(0), points(1)),
          (points(2), points(3)),
          (points(0), points(2)),
          (points(1), points(3)),

          (points(4), points(5)),
          (points(6), points(7)),
          (points(4), points(6)),
          (points(5), points(7)),

          (points(0), points(4)),
          (points(1), points(5)),
          (points(2), points(6)),
          (points(3), points(7))
        )
    }
  }
}
