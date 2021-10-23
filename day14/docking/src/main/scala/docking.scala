import scala.io.Source
import scala.collection.mutable
import scala.math.pow

object Docking extends App {
    if(args.size != 1) {
        println("Input file required!")
        System.exit(1)
    }

    val program = Source.fromFile(args(0)).getLines().toVector
    println(run(program).values.sum)
    println(run(program, true).values.sum)

    case class Mask(m1: Long, m0: Long)
    type Memory = Map[Long, Long]

    def run(lines: Vector[String], inverted: Boolean = false): Memory = {
        val mask = ""
        val memory: Memory = Map[Long, Long]()
        lines.foldLeft(mask, memory)(execute(inverted)(_, _))._2
    }

    def execute(inverted: Boolean)(state: (String, Memory), line: String): (String, Memory) = {
        val (mask, memory) = state
        line.split(" = ") match {
            case Array("mask", value) => (value, memory)
            case Array(address, value) => (mask,
                                           write(inverted, memory, parse_address(address), value.toLong, mask))
        }
    }

    def write(inverted: Boolean, memory: Memory, address: Long, value: Long, mask: String): Memory = {
        if (!inverted) memory + (address -> mask_value(value, parse_mask(mask)))
        else {
            val addresses_to_write = mask_address(address, parse_mask(mask))
            memory ++ addresses_to_write.map(_ -> value)
        }
    }

    def parse_mask(mask: String): Mask = {
        val m1 = mask.foldRight((0L, 0))({case (next, v) => (if (next == '1') v._1 + pow(2, v._2).toLong else v._1, v._2 + 1)})._1
        val m0 = mask.foldRight((0L, 0))({case (next, v) => (if (next == '0') v._1 + pow(2, v._2).toLong else v._1, v._2 + 1)})._1
        Mask(m1, m0)
    }

    def parse_address(address: String): Long = {
        address.drop(4).init.toLong
    }

    def mask_value(value: Long, mask: Mask): Long = {
        (value | mask.m1) & ~mask.m0
    }

    def mask_address(address: Long, mask: Mask): Vector[Long] = {
        val m1 = mask.m1 | (address & mask.m0)    // mX == ~m1 & ~m0; (x | m1) & ~mX == (x | m1) & ~(~m1 & ~m0) == (x | m1) & (m1 | m0) == m1 | (x & m0) 
        val m0 = ~address & mask.m0
        expand(Mask(m1, m0), 0)
    }

    def expand(mask: Mask, bit: Int): Vector[Long] = {
        if (bit > 35) {
            Vector(0L)
        }
        else {
            val expanded = expand(Mask(mask.m1 >> 1, mask.m0 >> 1), bit + 1)
            if ((mask.m1 & 1) == 1) {
                expanded.map(_ + pow(2, bit).toLong)
            }
            else if ((mask.m0 & 1) == 1) {
                expanded
            }
            else
                expanded.map(_ + pow(2, bit).toLong) ++ expanded
        }
    }
}