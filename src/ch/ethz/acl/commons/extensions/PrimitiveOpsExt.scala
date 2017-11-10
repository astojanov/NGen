package ch.ethz.acl.commons.extensions

import ch.ethz.acl.passera.unsigned._
import scala.lms.common._
import scala.reflect.SourceContext

trait PrimitiveOpsExt extends PrimitiveOpsExp with UnsignedPrimitiveOps{

  // ===================================================================================================================
  // Byte
  // ===================================================================================================================

  case class ByteMod               (lhs: Exp[Byte], rhs: Exp[Byte]) extends Def[Byte]
  case class ByteBinaryOr          (lhs: Exp[Byte], rhs: Exp[Byte]) extends Def[Byte]
  case class ByteBinaryAnd         (lhs: Exp[Byte], rhs: Exp[Byte]) extends Def[Byte]
  case class ByteBinaryXor         (lhs: Exp[Byte], rhs: Exp[Byte]) extends Def[Byte]
  case class ByteShiftLeft         (lhs: Exp[Byte], rhs: Exp[Byte]) extends Def[Byte]
  case class ByteShiftRightArith   (lhs: Exp[Byte], rhs: Exp[Byte]) extends Def[Byte]
  case class ByteShiftRightLogical (lhs: Exp[Byte], rhs: Exp[Byte]) extends Def[Byte]

  def infix_%  (lhs: Rep[Byte], rhs: Rep[Byte])(implicit o: Overloaded3, pos: SourceContext): Rep[Byte] = byte_mod(lhs, rhs)
  def infix_&  (lhs: Rep[Byte], rhs: Rep[Byte])(implicit o: Overloaded3, pos: SourceContext): Rep[Byte] = byte_binaryand(lhs, rhs)
  def infix_|  (lhs: Rep[Byte], rhs: Rep[Byte])(implicit o: Overloaded3, pos: SourceContext): Rep[Byte] = byte_binaryor(lhs, rhs)
  def infix_^  (lhs: Rep[Byte], rhs: Rep[Byte])(implicit o: Overloaded3, pos: SourceContext): Rep[Byte] = byte_binaryxor(lhs, rhs)
  def infix_<< (lhs: Rep[Byte], rhs: Rep[Byte])(implicit o: Overloaded3, pos: SourceContext): Rep[Byte] = byte_leftshift(lhs, rhs)
  def infix_>> (lhs: Rep[Byte], rhs: Rep[Byte])(implicit o: Overloaded3, pos: SourceContext): Rep[Byte] = byte_rightshiftarith(lhs, rhs)
  def infix_>>>(lhs: Rep[Byte], rhs: Rep[Byte])(implicit o: Overloaded3, pos: SourceContext): Rep[Byte] = byte_rightshiftlogical(lhs, rhs)

  def byte_mod               (lhs: Exp[Byte], rhs: Exp[Byte])(implicit pos: SourceContext): Exp[Byte] = ByteMod(lhs, rhs)
  def byte_binaryor          (lhs: Exp[Byte], rhs: Exp[Byte])(implicit pos: SourceContext): Exp[Byte] = ByteBinaryOr(lhs, rhs)
  def byte_binaryand         (lhs: Exp[Byte], rhs: Exp[Byte])(implicit pos: SourceContext): Exp[Byte] = ByteBinaryAnd(lhs, rhs)
  def byte_binaryxor         (lhs: Exp[Byte], rhs: Exp[Byte])(implicit pos: SourceContext): Exp[Byte] = ByteBinaryXor(lhs, rhs)
  def byte_leftshift         (lhs: Exp[Byte], rhs: Exp[Byte])(implicit pos: SourceContext): Exp[Byte] = ByteShiftLeft(lhs, rhs)
  def byte_rightshiftarith   (lhs: Exp[Byte], rhs: Exp[Byte])(implicit pos: SourceContext): Exp[Byte] = ByteShiftRightArith(lhs, rhs)
  def byte_rightshiftlogical (lhs: Exp[Byte], rhs: Exp[Byte])(implicit pos: SourceContext): Exp[Byte] = ByteShiftRightLogical(lhs, rhs)

  // ===================================================================================================================
  // UByte
  // ===================================================================================================================

  case class UByteMod               (lhs: Exp[UByte], rhs: Exp[UByte]) extends Def[UByte]
  case class UByteBinaryOr          (lhs: Exp[UByte], rhs: Exp[UByte]) extends Def[UByte]
  case class UByteBinaryAnd         (lhs: Exp[UByte], rhs: Exp[UByte]) extends Def[UByte]
  case class UByteBinaryXor         (lhs: Exp[UByte], rhs: Exp[UByte]) extends Def[UByte]
  case class UByteShiftLeft         (lhs: Exp[UByte], rhs: Exp[UByte]) extends Def[UByte]
  case class UByteShiftRightArith   (lhs: Exp[UByte], rhs: Exp[UByte]) extends Def[UByte]
  case class UByteShiftRightLogical (lhs: Exp[UByte], rhs: Exp[UByte]) extends Def[UByte]

  def infix_%  (lhs: Rep[UByte], rhs: Rep[UByte])(implicit o: Overloaded5, pos: SourceContext): Rep[UByte] = ubyte_mod(lhs, rhs)
  def infix_&  (lhs: Rep[UByte], rhs: Rep[UByte])(implicit o: Overloaded5, pos: SourceContext): Rep[UByte] = ubyte_binaryand(lhs, rhs)
  def infix_|  (lhs: Rep[UByte], rhs: Rep[UByte])(implicit o: Overloaded5, pos: SourceContext): Rep[UByte] = ubyte_binaryor(lhs, rhs)
  def infix_^  (lhs: Rep[UByte], rhs: Rep[UByte])(implicit o: Overloaded5, pos: SourceContext): Rep[UByte] = ubyte_binaryxor(lhs, rhs)
  def infix_<< (lhs: Rep[UByte], rhs: Rep[UByte])(implicit o: Overloaded5, pos: SourceContext): Rep[UByte] = ubyte_leftshift(lhs, rhs)
  def infix_>> (lhs: Rep[UByte], rhs: Rep[UByte])(implicit o: Overloaded5, pos: SourceContext): Rep[UByte] = ubyte_rightshiftarith(lhs, rhs)
  def infix_>>>(lhs: Rep[UByte], rhs: Rep[UByte])(implicit o: Overloaded5, pos: SourceContext): Rep[UByte] = ubyte_rightshiftlogical(lhs, rhs)

  def ubyte_mod               (lhs: Exp[UByte], rhs: Exp[UByte])(implicit pos: SourceContext): Exp[UByte] = UByteMod(lhs, rhs)
  def ubyte_binaryor          (lhs: Exp[UByte], rhs: Exp[UByte])(implicit pos: SourceContext): Exp[UByte] = UByteBinaryOr(lhs, rhs)
  def ubyte_binaryand         (lhs: Exp[UByte], rhs: Exp[UByte])(implicit pos: SourceContext): Exp[UByte] = UByteBinaryAnd(lhs, rhs)
  def ubyte_binaryxor         (lhs: Exp[UByte], rhs: Exp[UByte])(implicit pos: SourceContext): Exp[UByte] = UByteBinaryXor(lhs, rhs)
  def ubyte_leftshift         (lhs: Exp[UByte], rhs: Exp[UByte])(implicit pos: SourceContext): Exp[UByte] = UByteShiftLeft(lhs, rhs)
  def ubyte_rightshiftarith   (lhs: Exp[UByte], rhs: Exp[UByte])(implicit pos: SourceContext): Exp[UByte] = UByteShiftRightArith(lhs, rhs)
  def ubyte_rightshiftlogical (lhs: Exp[UByte], rhs: Exp[UByte])(implicit pos: SourceContext): Exp[UByte] = UByteShiftRightLogical(lhs, rhs)

  // ===================================================================================================================
  // UInt
  // ===================================================================================================================

  case class UIntMod               (lhs: Exp[UInt], rhs: Exp[UInt]) extends Def[UInt]
  case class UIntBinaryOr          (lhs: Exp[UInt], rhs: Exp[UInt]) extends Def[UInt]
  case class UIntBinaryAnd         (lhs: Exp[UInt], rhs: Exp[UInt]) extends Def[UInt]
  case class UIntBinaryXor         (lhs: Exp[UInt], rhs: Exp[UInt]) extends Def[UInt]
  case class UIntShiftLeft         (lhs: Exp[UInt], rhs: Exp[UInt]) extends Def[UInt]
  case class UIntShiftRightArith   (lhs: Exp[UInt], rhs: Exp[UInt]) extends Def[UInt]
  case class UIntShiftRightLogical (lhs: Exp[UInt], rhs: Exp[UInt]) extends Def[UInt]

  def infix_%  (lhs: Rep[UInt], rhs: Rep[UInt])(implicit o: Overloaded4, pos: SourceContext): Rep[UInt] = uint_mod(lhs, rhs)
  def infix_&  (lhs: Rep[UInt], rhs: Rep[UInt])(implicit o: Overloaded4, pos: SourceContext): Rep[UInt] = uint_binaryand(lhs, rhs)
  def infix_|  (lhs: Rep[UInt], rhs: Rep[UInt])(implicit o: Overloaded4, pos: SourceContext): Rep[UInt] = uint_binaryor(lhs, rhs)
  def infix_^  (lhs: Rep[UInt], rhs: Rep[UInt])(implicit o: Overloaded4, pos: SourceContext): Rep[UInt] = uint_binaryxor(lhs, rhs)
  def infix_<< (lhs: Rep[UInt], rhs: Rep[UInt])(implicit o: Overloaded4, pos: SourceContext): Rep[UInt] = uint_leftshift(lhs, rhs)
  def infix_>> (lhs: Rep[UInt], rhs: Rep[UInt])(implicit o: Overloaded4, pos: SourceContext): Rep[UInt] = uint_rightshiftarith(lhs, rhs)
  def infix_>>>(lhs: Rep[UInt], rhs: Rep[UInt])(implicit o: Overloaded4, pos: SourceContext): Rep[UInt] = uint_rightshiftlogical(lhs, rhs)

  def uint_mod               (lhs: Exp[UInt], rhs: Exp[UInt])(implicit pos: SourceContext): Exp[UInt] = UIntMod(lhs, rhs)
  def uint_binaryor          (lhs: Exp[UInt], rhs: Exp[UInt])(implicit pos: SourceContext): Exp[UInt] = UIntBinaryOr(lhs, rhs)
  def uint_binaryand         (lhs: Exp[UInt], rhs: Exp[UInt])(implicit pos: SourceContext): Exp[UInt] = UIntBinaryAnd(lhs, rhs)
  def uint_binaryxor         (lhs: Exp[UInt], rhs: Exp[UInt])(implicit pos: SourceContext): Exp[UInt] = UIntBinaryXor(lhs, rhs)
  def uint_leftshift         (lhs: Exp[UInt], rhs: Exp[UInt])(implicit pos: SourceContext): Exp[UInt] = UIntShiftLeft(lhs, rhs)
  def uint_rightshiftarith   (lhs: Exp[UInt], rhs: Exp[UInt])(implicit pos: SourceContext): Exp[UInt] = UIntShiftRightArith(lhs, rhs)
  def uint_rightshiftlogical (lhs: Exp[UInt], rhs: Exp[UInt])(implicit pos: SourceContext): Exp[UInt] = UIntShiftRightLogical(lhs, rhs)


  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    //
    // Byte
    //
    case ByteMod(x,y)                  => byte_mod(f(x),f(y))
    case ByteBinaryOr(x,y)             => byte_binaryor(f(x),f(y))
    case ByteBinaryAnd(x,y)            => byte_binaryand(f(x),f(y))
    case ByteBinaryXor(x,y)            => byte_binaryxor(f(x),f(y))
    case ByteShiftLeft(x,y)            => byte_leftshift(f(x),f(y))
    case ByteShiftRightLogical(x,y)    => byte_rightshiftlogical(f(x),f(y))
    case ByteShiftRightArith(x,y)      => byte_rightshiftarith(f(x),f(y))

    case Reflect(ByteMod(x,y)                , u, es) => reflectMirrored(Reflect(ByteMod(f(x),f(y))                 , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ByteBinaryOr(x,y)           , u, es) => reflectMirrored(Reflect(ByteBinaryOr(f(x),f(y))            , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ByteBinaryAnd(x,y)          , u, es) => reflectMirrored(Reflect(ByteBinaryAnd(f(x),f(y))           , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ByteBinaryXor(x,y)          , u, es) => reflectMirrored(Reflect(ByteBinaryXor(f(x),f(y))           , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ByteShiftLeft(x,y)          , u, es) => reflectMirrored(Reflect(ByteShiftLeft(f(x),f(y))           , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ByteShiftRightLogical(x,y)  , u, es) => reflectMirrored(Reflect(ByteShiftRightLogical(f(x),f(y))   , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ByteShiftRightArith(x,y)    , u, es) => reflectMirrored(Reflect(ByteShiftRightArith(f(x),f(y))     , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    //
    // UByte
    //
    case UByteMod(x,y)                  => ubyte_mod(f(x),f(y))
    case UByteBinaryOr(x,y)             => ubyte_binaryor(f(x),f(y))
    case UByteBinaryAnd(x,y)            => ubyte_binaryand(f(x),f(y))
    case UByteBinaryXor(x,y)            => ubyte_binaryxor(f(x),f(y))
    case UByteShiftLeft(x,y)            => ubyte_leftshift(f(x),f(y))
    case UByteShiftRightLogical(x,y)    => ubyte_rightshiftlogical(f(x),f(y))
    case UByteShiftRightArith(x,y)      => ubyte_rightshiftarith(f(x),f(y))

    case Reflect(UByteMod(x,y)                , u, es) => reflectMirrored(Reflect(UByteMod(f(x),f(y))                 , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UByteBinaryOr(x,y)           , u, es) => reflectMirrored(Reflect(UByteBinaryOr(f(x),f(y))            , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UByteBinaryAnd(x,y)          , u, es) => reflectMirrored(Reflect(UByteBinaryAnd(f(x),f(y))           , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UByteBinaryXor(x,y)          , u, es) => reflectMirrored(Reflect(UByteBinaryXor(f(x),f(y))           , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UByteShiftLeft(x,y)          , u, es) => reflectMirrored(Reflect(UByteShiftLeft(f(x),f(y))           , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UByteShiftRightLogical(x,y)  , u, es) => reflectMirrored(Reflect(UByteShiftRightLogical(f(x),f(y))   , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UByteShiftRightArith(x,y)    , u, es) => reflectMirrored(Reflect(UByteShiftRightArith(f(x),f(y))     , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    //
    // UInt
    //
    case UIntMod(x,y)                  => uint_mod(f(x),f(y))
    case UIntBinaryOr(x,y)             => uint_binaryor(f(x),f(y))
    case UIntBinaryAnd(x,y)            => uint_binaryand(f(x),f(y))
    case UIntBinaryXor(x,y)            => uint_binaryxor(f(x),f(y))
    case UIntShiftLeft(x,y)            => uint_leftshift(f(x),f(y))
    case UIntShiftRightLogical(x,y)    => uint_rightshiftlogical(f(x),f(y))
    case UIntShiftRightArith(x,y)      => uint_rightshiftarith(f(x),f(y))

    case Reflect(UIntMod(x,y)                , u, es) => reflectMirrored(Reflect(UIntMod(f(x),f(y))                 , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UIntBinaryOr(x,y)           , u, es) => reflectMirrored(Reflect(UIntBinaryOr(f(x),f(y))            , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UIntBinaryAnd(x,y)          , u, es) => reflectMirrored(Reflect(UIntBinaryAnd(f(x),f(y))           , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UIntBinaryXor(x,y)          , u, es) => reflectMirrored(Reflect(UIntBinaryXor(f(x),f(y))           , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UIntShiftLeft(x,y)          , u, es) => reflectMirrored(Reflect(UIntShiftLeft(f(x),f(y))           , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UIntShiftRightLogical(x,y)  , u, es) => reflectMirrored(Reflect(UIntShiftRightLogical(f(x),f(y))   , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(UIntShiftRightArith(x,y)    , u, es) => reflectMirrored(Reflect(UIntShiftRightArith(f(x),f(y))     , mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait CGenPrimitiveOpsExt extends CLikeGenBase {
  val IR: PrimitiveOpsExt
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    //
    // Byte
    //
    case ByteMod              (lhs, rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case ByteBinaryOr         (lhs, rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case ByteBinaryAnd        (lhs, rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case ByteBinaryXor        (lhs, rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case ByteShiftLeft        (lhs, rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case ByteShiftRightArith  (lhs, rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case ByteShiftRightLogical(lhs, rhs) => emitValDef(sym, "(uint8_t)" + quote(lhs) + " >> " + quote(rhs))
    //
    // UByte
    //
    case UByteMod              (lhs, rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case UByteBinaryOr         (lhs, rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case UByteBinaryAnd        (lhs, rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case UByteBinaryXor        (lhs, rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case UByteShiftLeft        (lhs, rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case UByteShiftRightArith  (lhs, rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case UByteShiftRightLogical(lhs, rhs) => emitValDef(sym, "(uint8_t)" + quote(lhs) + " >> " + quote(rhs))//
    // UInt
    //
    case UIntMod              (lhs, rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case UIntBinaryOr         (lhs, rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case UIntBinaryAnd        (lhs, rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case UIntBinaryXor        (lhs, rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case UIntShiftLeft        (lhs, rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case UIntShiftRightArith  (lhs, rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case UIntShiftRightLogical(lhs, rhs) => emitValDef(sym, "(uint32_t)" + quote(lhs) + " >> " + quote(rhs))

    case _ => super.emitNode(sym, rhs)
  }
}

