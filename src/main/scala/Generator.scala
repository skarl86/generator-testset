/**
  * Created by NK on 2017. 4. 21..
  */

import java.io._

object Generator {
  def main(args: Array[String]) {
//    val count:Long = args(0).toLong
    val count:Int = 1000
    var triple:String = create()
    for (i <- 1 to count){
      triple = List(triple, create()).mkString("\n")
    }
    writeFile("test_set"+count,triple)
  }

  def writeFile(fileName:String, text:String) = {
    val file = new File(fileName+".nt")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }
  def create(): String = {
    val uniqueID:Long = java.lang.System.currentTimeMillis()

    val parent1:String = "parent" + uniqueID
    val parent2:String = "parent" + (uniqueID + 1)
    val child1:String = "child" + (uniqueID + 2)

    val parent1Triple = createParentTriple(child1, parent1)
    val parent2Triple = createParentTriple(child1, parent2)
    val fatherTriple = createFather(child1, parent1)
    val motherTriple = createMother(child1, parent2)

    return List(parent1Triple, parent2Triple, fatherTriple, motherTriple).mkString("\n")

  }
  def createParentTriple(child:String, parent:String): String ={
    val s:String = createResourceURI(child)
    val p:String = "<http://xb.saltlux.com/schema/property/parent>"
    val o:String = createResourceURI(parent)

    return createTriple(s, p, o)
  }
  def createFather(child:String, parent:String): String = {
    // s: <http://xb.saltlux.com/resource/0000551007>
    // p: <http://xb.saltlux.com/schema/property/spouse>
    // o: <http://xb.saltlux.com/resource/0000066942> .
    val s: String = createResourceURI(child)
    val p: String = "<http://xb.saltlux.com/schema/property/father>"
    val o: String = createResourceURI(parent)

    return createTriple(s, p, o)
  }

  def createMother(child:String, parent:String): String = {
    // s: <http://xb.saltlux.com/resource/0000551007>
    // p: <http://xb.saltlux.com/schema/property/spouse>
    // o: <http://xb.saltlux.com/resource/0000066942> .
    val s: String = createResourceURI(child)
    val p: String = "<http://xb.saltlux.com/schema/property/mother>"
    val o: String = createResourceURI(parent)

    return createTriple(s, p, o)
  }

  def createSpouse(parent1:String, parent2:String): String = {
    // s: <http://xb.saltlux.com/resource/0000551007>
    // p: <http://xb.saltlux.com/schema/property/spouse>
    // o: <http://xb.saltlux.com/resource/0000066942> .
    val s:String = createResourceURI(parent1)
    val p:String = "<http://xb.saltlux.com/schema/property/spouse>"
    val o:String = createResourceURI(parent2)

    return createTriple(s, p, o)
  }

  def createResourceURI(id:String): String ={
    return "<http://xb.saltlux.com/resource/" + id + ">"
  }

  def createTriple(s:String, p:String, o:String): String = {
    return List(s, p, o).mkString(" ") + " ."
  }
}
