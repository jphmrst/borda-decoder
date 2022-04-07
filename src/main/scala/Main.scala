import scala.collection.mutable.{Builder, HashMap, HashSet}
import java.io.File
import com.github.tototoshi.csv.*
import org.maraist.latex.{LaTeXdoc, Sampler}

val src = "/home/jm/DFL/SD63/CaucusConvention/borda-decoder/raw-results.csv"
val candidateCount = 16

@main def mainResponseProcessor: Unit = {
  println(s"Reading results from file $src")
  val reader = CSVReader.open(new File(src))
  val responses = getResponses(reader)
  reader.close
  printVCresults(responses)
  val bordaCounts = getBordaCounts(responses)
  // fullEcho(responses, bordaCounts)
  writeCSV(responses, bordaCounts)
}

def getResponses(reader: CSVReader): Seq[Response] = {
  val rawResps = reader.all().tail.map(Response(_))

  println(s"\nFound ${rawResps.length} ballot(s)")
  for (resp <- rawResps) do {
    val count = rawResps.filter(_.email == resp.email).length
    println(s"From ${resp.email} ($count with this address) at ${resp.date}")
  }

  println(s"\nRemoving any replaced ballots")
  val isLastWithEmail = (resp: Response) => {
    val thisEmail = resp.email
    val ifThisEmail = (x: Response) => x.email == thisEmail
    val lastIdx = rawResps.lastIndexWhere(ifThisEmail)
    resp == rawResps(lastIdx)
  }
  val resps = rawResps.filter(isLastWithEmail)
  for (resp <- resps) do {
    val count = resps.filter(_.email == resp.email).length
    println(s"From ${resp.email} ($count with this address) at ${resp.date}")
  }

  resps
}

def fullEcho(resps: Seq[Response], counts: Map[Candidate, Int]): Unit = {
  for (resp <- resps) do {
    println(resp)
  }
  for (cand <- Candidate.sorted(counts)) do {
    println(s"  ${cand.name}, ${counts(cand)}")
  }
}

def printVCresults(resps: Seq[Response]): Unit = {
  val selected: Set[String] = {
    val buf = Set.newBuilder[String]
    for (resp <- resps) do {
      buf += resp.viceChairVote
    }
    buf.result
  }

  println("\nVice-Chair results: ")
  for (vc <- selected) do {
    val count = resps.filter(_.viceChairVote == vc).length
    println(s"  $count - $vc")
  }
}

def writeCSV(resps: Seq[Response], counts: Map[Candidate, Int]): Unit = {
  val writer = CSVWriter.open("tally.csv")
  println("\nCreating file tally.csv for delegate rankings")

  // The line with responder emails
  val respEmailLine = List.newBuilder[String]
  respEmailLine += ""
  for (resp <- resps)
    do if !nonrankerEmails.contains(resp.email) then
      respEmailLine += resp.email
  writer.writeRow(respEmailLine.result)

  // Now for each candidate, the rank from each responder for that
  // candidate
  for (cand <- Candidate.all) do {
    val candRankLine = List.newBuilder[String]
    candRankLine += cand.name
    for (resp <- resps) do if !nonrankerEmails.contains(resp.email) then {
      resp.forCandidate.get(cand) match {
        case Some(rank) => candRankLine += rank.toString()
        case None => candRankLine += ""
      }
    }
    writer.writeRow(candRankLine.result)
  }

  // Blank line
  writer.writeRow(List())

  for (cand <- Candidate.sorted(counts)) do {
    writer.writeRow(List(cand.name, counts(cand)))
  }
  writer.close
  println(" - Written")
  println()
}

val nonrankerEmails = Seq(
  "jettieann@gmail.com",
  "mlickne1@yahoo.com"
)

def getBordaCounts(resps: Seq[Response]): Map[Candidate, Int] = {
  val buf = new HashMap[Candidate, Int]
  for (resp <- resps) do {
    // println(s"\"${resp.email}\"")
    if !nonrankerEmails.contains(resp.email) then {
      // println("  Y")
      for (i <- 1 to candidateCount) do {
        resp.ranks(i).map((who) => {
          val prev = buf.getOrElse(who, 0)
          val rank = prev + candidateCount + 1 - i
          buf(who) = rank
        })
      }
    }
  }
  Map.from(buf)
}

// =============== Response wrapper

case class Response(
  date: String,
  email: String,
  viceChairVote: String,
  ranks: Array[Option[Candidate]],
  forCandidate: Map[Candidate, Int]
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
        /* :: aRank :: bRank :: cRank :: dRank :: eRank :: fRank :: gRank :: hRank */
           :: hogan :: riordan :: doherty :: howard :: hill :: jones :: keller
           :: valen :: whiting :: lifson :: bush :: bonham :: vandervegte
           :: abramson :: hinds :: ferguson
        :: _ => {
          val ranks: Array[Option[Candidate]] =
            Array.fill(candidateCount+1)(None)
          val forCandidate = Map.newBuilder[Candidate, Int]

          /*
          addRanking(ranks, forCandidate, Candidate.AA, aRank)
          addRanking(ranks, forCandidate, Candidate.BB, bRank)
          addRanking(ranks, forCandidate, Candidate.CC, cRank)
          addRanking(ranks, forCandidate, Candidate.DD, dRank)
          addRanking(ranks, forCandidate, Candidate.EE, eRank)
          addRanking(ranks, forCandidate, Candidate.FF, fRank)
          addRanking(ranks, forCandidate, Candidate.GG, gRank)
          addRanking(ranks, forCandidate, Candidate.HH, hRank)
          */

          addRanking(ranks, forCandidate, Candidate.JulieDoherty, doherty)
          addRanking(ranks, forCandidate, Candidate.MichaelAbramson, abramson)
          addRanking(ranks, forCandidate, Candidate.TimBonham, bonham)
          addRanking(ranks, forCandidate, Candidate.JimBush, bush)
          addRanking(ranks, forCandidate, Candidate.EricFerguson, ferguson)
          addRanking(ranks, forCandidate, Candidate.BurkeHinds, hinds)
          addRanking(ranks, forCandidate, Candidate.TommyKeller, keller)
          addRanking(ranks, forCandidate, Candidate.AlanLifson, lifson)
          addRanking(ranks, forCandidate, Candidate.AlexValen, valen)
          addRanking(
            ranks, forCandidate, Candidate.NickVanderVegte, vandervegte)
          addRanking(ranks, forCandidate, Candidate.JamisonWhiting, whiting)
          addRanking(ranks, forCandidate, Candidate.JettieAnnHill, hill)
          addRanking(ranks, forCandidate, Candidate.DevinHogan, hogan)
          addRanking(ranks, forCandidate, Candidate.MollyHoward, howard)
          addRanking(ranks, forCandidate, Candidate.AnneJones, jones)
          addRanking(ranks, forCandidate, Candidate.AprilRiordan, riordan)

          Response(timestamp, email, viceChairVote, ranks, forCandidate.result)
        }
    case _ => throw new IllegalArgumentException(tuple.toString)
  }


  def addRanking(
    ranks: Array[Option[Candidate]],
    forCand: Builder[(Candidate, Int), Map[Candidate, Int]],
    who: Candidate, rank: String
  ) = {
    decodeRank(rank).map((i) => {
      ranks(i) = Some(who)
      forCand += ((who, i))
    })
  }
}
// =============== The candidates

enum Candidate(val name: String) {
  /*
  case AA extends Candidate("AA")
  case BB extends Candidate("BB")
  case CC extends Candidate("CC")
  case DD extends Candidate("DD")
  case EE extends Candidate("EE")
  case FF extends Candidate("FF")
  case GG extends Candidate("GG")
  case HH extends Candidate("HH")
   */
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
}

object Candidate {
  /*
  def all = List(AA,BB,CC,DD,EE,FF,GG,HH)
   */
  def all = List(
    DevinHogan,
    AprilRiordan, JulieDoherty, MollyHoward, JettieAnnHill, AnneJones,
    TommyKeller, AlexValen, JamisonWhiting,
    AlanLifson, JimBush, TimBonham, NickVanderVegte,
    MichaelAbramson, BurkeHinds, EricFerguson)
  def sorted(scores: Map[Candidate,Int]): List[Candidate] = {
    given Ordering[Candidate] = new Ordering[Candidate] {
      def compare(a: Candidate, b: Candidate) = scores(b).compare(scores(a))
    }
    all.sorted
  }
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
