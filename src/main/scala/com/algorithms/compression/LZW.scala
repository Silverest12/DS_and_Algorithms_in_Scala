package com.algorithms.compression

//I have to refractor that joke of a code
// but Hey it works !
object LZW {
  def compress (text: String): List[Int] = {
    val startDic = (1 to 255).map(a=>(""+a.toChar, a)).toMap
    val (fullDict, result, remain) = text.foldLeft ((startDic, List[Int](), "")) {
      case ((dict, res, leftOver), nextChar) =>
        if (dict.contains(leftOver + nextChar))
          (dict, res, leftOver+nextChar)
        else if (dict.size < 4096)
          (dict + ((leftOver + nextChar, dict.size + 1)), dict(leftOver) :: res, ""+nextChar)
        else
          (dict, dict(leftOver) :: res, ""+nextChar)
    }
    if (remain.isEmpty) result.reverse else (fullDict(remain) :: result).reverse
  }

  def decompress (map: List[Int]): String = {
    val startDict = (1 to 255).map(a=>(a, ""+a.toChar)).toMap
    val (_, result, _) = {
      map.foldLeft[(Map[Int, String], List[String], Option[(Int, String)])] ((startDict, Nil, None)) {
        case  ((dict, result, conjecture), n) => {
          dict.get(n) match {
            case Some (output) => {
              val (newDict, newCode) = conjecture match {
                case Some((code, prefix)) => (dict + (code -> (prefix + output.head)), code + 1)
                case None =>  (dict, dict.size + 1)
              }
              (newDict, output :: result, Some(newCode -> output))
            }
            case None => {
              val (code, prefix) = conjecture.get
              val output = prefix + prefix.head
              (dict + (code -> output), output :: result, Some(code + 1 -> output))
            }
          }
        }
      }
    }
    result.reverse.mkString
  }
}
