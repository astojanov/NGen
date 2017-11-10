package ch.ethz.acl.commons.extensions

import ch.ethz.acl.passera.unsigned._
import scala.lms.common.BaseExp

trait UnsignedPrimitiveOps extends BaseExp {
  implicit def anyTyp   : Typ[Any]    = manifestTyp[Any]
  implicit def uByteTyp : Typ[UByte]  = manifestTyp[UByte]
  implicit def uShortTyp: Typ[UShort] = manifestTyp[UShort]
  implicit def uIntTyp  : Typ[UInt]   = manifestTyp[UInt]
  implicit def uLongTyp : Typ[ULong]  = manifestTyp[ULong]
}

