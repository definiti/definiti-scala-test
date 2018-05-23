package definiti.native

sealed trait Message

case class Message0(key: String) extends Message
case class Message1[P1](key: String, p1: P1) extends Message
case class Message2[P1, P2](key: String, p1: P1, p2: P2) extends Message
case class Message3[P1, P2, P3](key: String, p1: P1, p2: P2, p3: P3) extends Message
case class Message4[P1, P2, P3, P4](key: String, p1: P1, p2: P2, p3: P3, p4: P4) extends Message
case class Message5[P1, P2, P3, P4, P5](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5) extends Message
case class Message6[P1, P2, P3, P4, P5, P6](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6) extends Message
case class Message7[P1, P2, P3, P4, P5, P6, P7](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7) extends Message
case class Message8[P1, P2, P3, P4, P5, P6, P7, P8](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8) extends Message
case class Message9[P1, P2, P3, P4, P5, P6, P7, P8, P9](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9) extends Message
case class Message10[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10) extends Message
case class Message11[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11) extends Message
case class Message12[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12) extends Message
case class Message13[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13) extends Message
case class Message14[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14) extends Message
case class Message15[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15) extends Message
case class Message16[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16) extends Message
case class Message17[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16, p17: P17) extends Message
case class Message18[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16, p17: P17, p18: P18) extends Message
case class Message19[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16, p17: P17, p18: P18, p19: P19) extends Message
case class Message20[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16, p17: P17, p18: P18, p19: P19, p20: P20) extends Message
case class Message21[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16, p17: P17, p18: P18, p19: P19, p20: P20, p21: P21) extends Message

object Message {
  def apply(key: String): Message =
    Message0(key)
  def apply[P1](key: String, p1: P1): Message =
    Message1(key, p1)
  def apply[P1, P2](key: String, p1: P1, p2: P2): Message =
    Message2(key, p1, p2)
  def apply[P1, P2, P3](key: String, p1: P1, p2: P2, p3: P3): Message =
    Message3(key, p1, p2, p3)
  def apply[P1, P2, P3, P4](key: String, p1: P1, p2: P2, p3: P3, p4: P4): Message =
    Message4(key, p1, p2, p3, p4)
  def apply[P1, P2, P3, P4, P5](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5): Message =
    Message5(key, p1, p2, p3, p4, p5)
  def apply[P1, P2, P3, P4, P5, P6](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6): Message =
    Message6(key, p1, p2, p3, p4, p5, p6)
  def apply[P1, P2, P3, P4, P5, P6, P7](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7): Message =
    Message7(key, p1, p2, p3, p4, p5, p6, p7)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8): Message =
    Message8(key, p1, p2, p3, p4, p5, p6, p7, p8)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9): Message =
    Message9(key, p1, p2, p3, p4, p5, p6, p7, p8, p9)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10): Message =
    Message10(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11): Message =
    Message11(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12): Message =
    Message12(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13): Message =
    Message13(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14): Message =
    Message14(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15): Message =
    Message15(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16): Message =
    Message16(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16, p17: P17): Message =
    Message17(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16, p17: P17, p18: P18): Message =
    Message18(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16, p17: P17, p18: P18, p19: P19): Message =
    Message19(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16, p17: P17, p18: P18, p19: P19, p20: P20): Message =
    Message20(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
  def apply[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](key: String, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12, p13: P13, p14: P14, p15: P15, p16: P16, p17: P17, p18: P18, p19: P19, p20: P20, p21: P21): Message =
    Message21(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
}
