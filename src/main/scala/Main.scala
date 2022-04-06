import scala.collection.mutable.{Builder, HashMap, HashSet}
import java.io.File
import com.github.tototoshi.csv.*
import org.maraist.latex.{LaTeXdoc, Sampler}

val src = "/home/jm/DFL/SD63/CaucusConvention/borda-decoder/test.csv"
val candidateCount = 8

@main def mainResponseProcessor: Unit = {
  println()
  val reader = CSVReader.open(new File(src))
  val responses = getResponses(reader)
  val bordaCounts = getBordaCounts(responses)
  fullEcho(responses, bordaCounts)

  println()
}

def getResponses(reader: CSVReader): Seq[Response] =
  reader.all().tail.map(Response(_))

def fullEcho(resps: Seq[Response], counts: Map[Candidate, Int]): Unit = {
  for (resp <- resps) do {
    println(resp)
  }
  for (cand <- Candidate.sorted(counts)) do {
    println(s"  ${cand.name}, ${counts(cand)}")
  }
}

def getBordaCounts(resps: Seq[Response]): Map[Candidate, Int] = {
  val buf = new HashMap[Candidate, Int]
  for (resp <- resps; i <- 1 to candidateCount) do {
    resp.ranks(i).map((who) => {
      val prev = buf.getOrElse(who, 0)
      val rank = prev + candidateCount + 1 - i
      buf(who) = rank
    })
  }
  Map.from(buf)
}

// =============== Response wrapper

case class Response(
  email: String,
  viceChairVote: String,
  ranks: Array[Option[Candidate]]
) {

  override def toString(): String = {
    val buf = new StringBuilder
    buf ++= s"$email\n  VC: $viceChairVote\n  Delegates:\n"
    for(i <- 1 to candidateCount) do {
      buf ++= s"    $i. "
      buf ++= (ranks(i) match {
        case Some(who) => who.name
        case None => "-"
      })
      buf ++= "\n"
    }
    buf.result
  }

}

object Response {
  def apply(tuple: List[String]): Response = tuple match {
    case timestamp :: email :: viceChairVote
        :: aRank :: bRank :: cRank :: dRank :: eRank :: fRank :: gRank :: hRank
        :: _ => {
          val ranks: Array[Option[Candidate]] =
            Array.fill(candidateCount+1)(None)
          addRanking(ranks, Candidate.AA, aRank)
          addRanking(ranks, Candidate.BB, bRank)
          addRanking(ranks, Candidate.CC, cRank)
          addRanking(ranks, Candidate.DD, dRank)
          addRanking(ranks, Candidate.EE, eRank)
          addRanking(ranks, Candidate.FF, fRank)
          addRanking(ranks, Candidate.GG, gRank)
          addRanking(ranks, Candidate.HH, hRank)
          Response(email, viceChairVote, ranks)
        }
    case _ => throw new IllegalArgumentException(tuple.toString)
  }
}

def addRanking(
  ranks: Array[Option[Candidate]], who: Candidate, rank: String
) = {
  decodeRank(rank).map((i) => { ranks(i) = Some(who) })
}
// =============== The candidates

enum Candidate(val name: String) {
  case AA extends Candidate("AA")
  case BB extends Candidate("BB")
  case CC extends Candidate("CC")
  case DD extends Candidate("DD")
  case EE extends Candidate("EE")
  case FF extends Candidate("FF")
  case GG extends Candidate("GG")
  case HH extends Candidate("HH")
  /*
  case JulieDoherty extends Candidate("Julie Doherty")
  case MichaelAbramson extends Candidate("Michael Abramson")
  case TimBonham extends Candidate("Tim Bonham")
  case JimBush extends Candidate("Jim Bush")
  case EricFerguson extends Candidate("Eric Ferguson")
  case BurkeHinds extends Candidate("Burke Hinds")
  case TommyKeller extends Candidate("Tommy Keller")
  case AlanLifson extends Candidate("Alan Lifson")
  case AlexValen extends Candidate("Alex Valen")
  case NickVanderVegte extends Candidate("Nick VanderVegte")
  case JamisonWhiting extends Candidate("Jamison Whiting")
  case JettieAnnHill extends Candidate("Jettie Ann Hill")
  case DevinHogan extends Candidate("Devin Hogan")
  case MollyHoward extends Candidate("Molly Howard")
  case AnneJones extends Candidate("Anne Jones")
  case AprilRiordan extends Candidate("April Riordan")
   */
}

object Candidate {
  def all = List(AA,BB,CC,DD,EE,FF,GG,HH)
  def sorted(scores: Map[Candidate,Int]): List[Candidate] = {
    given Ordering[Candidate] = new Ordering[Candidate] {
      def compare(a: Candidate, b: Candidate) = scores(b).compare(scores(a))
    }
    all.sorted
  }
  /*
  def all = Set(
    DevinHogan,
    AprilRiordan, JulieDoherty, MollyHoward, JettieAnnHill, AnneJones,
    TommyKeller, AlexValen, JamisonWhiting,
    AlanLifson, JimBush, TimBonham, NickVanderVegte,
    MichaelAbramson, BurkeHinds, EricFerguson)
   */
}

// =============== Rank

def decodeRank(str: String): Option[Int] = str match {
  case "" => None
  case "Top rank" => Some(1)
  case "2nd rank" => Some(2)
  case "3rd" => Some(3)
  case "4th" => Some(4)
  case "5th" => Some(5)
  case "6th" => Some(6)
  case "7th" => Some(7)
  case "8th" => Some(8)
  case "9th" => Some(9)
  case "10th" => Some(10)
  case "11th" => Some(11)
  case "12th" => Some(12)
  case "13th" => Some(13)
  case "14th" => Some(14)
  case "15th" => Some(15)
  case "16th" => Some(16)
  case "17th" => Some(17)
  case "18th" => Some(18)
  case "19th" => Some(19)
  case "20th" => Some(20)
  case _ => throw new IllegalArgumentException(s"No such ranking $str")
}
