/**
  * Created by NK on 2017. 4. 21..
  */

import java.io._

import scala.collection.mutable.ListBuffer

object Generator {
  def main(args: Array[String]) {
//    val count:Long = args(0).toLong
    val count:Int = 1
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

    val parent1:String = "parent_" + uniqueID
    val parent2:String = "parent_" + (uniqueID + 1)
    val grandParent1:String = "grandparent_" + uniqueID
    val grandParent2:String = "grandparent_" + (uniqueID + 1)
    val child1:String = "child_" + (uniqueID + 2)
    val child2:String = "child_" + (uniqueID + 3)

    val state1:String = "state_" + uniqueID
    val state2:String = "state_" + (uniqueID + 1)
    val city1:String = "city_" + uniqueID
    val city2:String = "city_" + (uniqueID + 1)
    val lang:String ="language_" + uniqueID

    val show:String = "show_" + uniqueID
    val painter:String = "painter_" + uniqueID

    val education:String ="education_" +uniqueID

    val person1:String ="person_" + uniqueID
    val person2:String ="person_" + (uniqueID+1)
    val person3:String ="person_" + (uniqueID+2)
    val person4:String ="person_" + (uniqueID+3)

    var triples = new ListBuffer[String]()
    triples += createPersonTypeTriple(parent1)
    triples += createPersonTypeTriple(parent2)
    triples += createPersonTypeTriple(child1)
    triples += createPersonTypeTriple(child2)

    triples += createParentTriple(child1, parent1)
    triples += createParentTriple(child1, parent2)
    triples += createParentTriple(child2, parent1)
    triples += createParentTriple(child2, parent2)

    triples += createFather(child1, parent1)
    triples += createMother(child1, parent2)
    triples += createFather(child2, parent1)
    triples += createMother(child2, parent2)

    triples += createChildTriple(parent1, child1)
    triples += createChildTriple(parent2, child1)
    triples += createChildTriple(parent1, child2)
    triples += createChildTriple(parent2, child2)

    // For query3
    triples += createSp_withIn(city1, city2)
    triples += createBornIn(parent1, city1)
    triples += createBornIn(parent2, city2)
    triples += createBornIn(child1, city1)
    triples += createBornIn(child2, city1)

    // For query4
    triples += createPersonTypeTriple(painter)
    triples += createDirectorTriple(show, painter)
    triples += createPaintingTypeTriple(painter)

    // For query5
    // xbp:capital(?e, ?a) ^ xbp:language(?e, ?b) -> xbp:language(?a, ?b)
    triples += createCapitalTriple(state1, city1)
    triples += createCapitalTriple(state2, city2)
    triples += createLanguageTriple(city1, lang)
    triples += createLanguageTriple(city2, lang)
    triples += createLanguageTriple(state1, lang)

    // For query6
    triples += createEducationTriple(child1, education)
    triples += createEducationTriple(child2, education)

    // For query7
    triples += createWife(parent1, parent2)

    // For query8
    triples += createStateTypeTriple(state1)
    triples += createStateTypeTriple(state2)
    triples += createLocatedIn(city1, state1)
    triples += createLocatedIn(city2, state2)
    triples += createSp_touches(state1, state2)

    // For query9
    triples += createCreationTriple(parent1, show)
    triples += createBornIn(painter,city1)
    triples += createShowTypeTriple(show)
    triples += createSisterTriple(person1, person2)
    triples += createSisterTriple(person3, person1)

    // For query10
    triples += createPersonTypeTriple(person1)
    triples += createPersonTypeTriple(person2)
    triples += createPersonTypeTriple(person3)
    triples += createPersonTypeTriple(person4)
    triples += createFather(person1, person2)
    triples += createMother(person2, person3)
    triples += createSisterTriple(person3, person4)

    return triples.distinct.toList.mkString("\n")

  }
  def createStateTypeTriple(state:String): String ={
    val s:String = createResourceURI(state)
    val p:String = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
    val o:String = "<http://xb.saltlux.com/schema/class/state_07673557>"

    return createTriple(s, p, o)
  }
  def createPersonTypeTriple(person:String): String = {
    val s:String = createResourceURI(person)
    val p:String = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
    val o:String = "<http://xb.saltlux.com/schema/class/person_00006026>"

    return createTriple(s, p, o)
  }

  def createShowTypeTriple(show:String): String = {
    val s:String = createResourceURI(show)
    val p:String = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
    val o:String = "<http://xb.saltlux.com/schema/class/show_06210248>"

    return createTriple(s, p, o)
  }

  def createPaintingTypeTriple(imaginary:String): String = {
    val s:String = createResourceURI(imaginary)
    val p:String = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
    val o:String = "<http://xb.saltlux.com/schema/class/painting_03730240>"

    return createTriple(s, p, o)
  }

  def createSisterTriple(person1:String, person2:String): String = {
    val s:String = createResourceURI(person1)
    val p:String = "<http://xb.saltlux.com/schema/property/sister>"
    val o:String = createResourceURI(person2)

    return createTriple(s, p, o)
  }
  def createCreationTriple(person:String, show:String): String = {
    val s:String = createResourceURI(person)
    val p:String = "<http://xb.saltlux.com/schema/property/creation>"
    val o:String = createResourceURI(show)

    return createTriple(s, p, o)
  }
  def createLocatedIn(city:String, state:String): String = {
    val s:String = createResourceURI(city)
    val p:String = "<http://xb.saltlux.com/schema/property/locatedIn>"
    val o:String = createResourceURI(state)

    return createTriple(s, p, o)
  }
  def createSp_touches(state1:String, state2:String): String ={
    val s:String = createResourceURI(state1)
    val p:String = "<http://xb.saltlux.com/schema/property/sp_touches>"
    val o:String = createResourceURI(state2)

    return createTriple(s, p, o)
  }
  def createWife(person1:String, person2:String):String ={
    val s:String = createResourceURI(person1)
    val p:String = "<http://xb.saltlux.com/schema/property/wife>"
    val o:String = createResourceURI(person2)

    return createTriple(s, p, o)
  }
  def createEducationTriple(person:String, education:String):String = {
    val s:String = createResourceURI(person)
    val p:String = "<http://xb.saltlux.com/schema/property/education>"
    val o:String = createResourceURI(education)

    return createTriple(s, p, o)
  }
  def createLanguageTriple(state:String, lang:String): String = {
    val s:String = createResourceURI(state)
    val p:String = "<http://xb.saltlux.com/schema/property/language>"
    val o:String = createResourceURI(lang)

    return createTriple(s, p, o)
  }
  def createCapitalTriple(state:String, city:String): String = {
    val s:String = createResourceURI(state)
    val p:String = "<http://xb.saltlux.com/schema/property/capital>"
    val o:String = createResourceURI(city)

    return createTriple(s, p, o)
  }

  def createDirectorTriple(show:String, imaginary:String): String = {
    val s:String = createResourceURI(show)
    val p:String = "<http://xb.saltlux.com/schema/property/director>"
    val o:String = createResourceURI(imaginary)

    return createTriple(s, p, o)
  }

  def createBornIn(person:String, geo:String): String = {
    val s:String = createResourceURI(person)
    val p:String = "<http://xb.saltlux.com/schema/property/bornIn>"
    val o:String = createResourceURI(geo)

    return createTriple(s, p ,o)
  }

  def createSp_withIn(geo1:String, geo2:String): String = {
    val s:String = createResourceURI(geo1)
    val p:String = "<http://xb.saltlux.com/schema/property/sp_withIn>"
    val o:String = createResourceURI(geo2)

    return createTriple(s, p ,o)
  }
  def createChildTriple(parent:String, child:String): String ={
    val s:String = createResourceURI(parent)
    val p:String = "<http://xb.saltlux.com/schema/property/child>"
    val o:String = createResourceURI(child)

    return createTriple(s, p ,o)
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
