/*

Q2. The Student class should contain one more field this time, gender. The values of gender must be set in a Enumeration. 

Write a method getScoreCardByGender to return a tuple of ScoreCards (e.g. (List[ScoreCard], List[ScoreCard])), where first field in the tuple has male student's score card and the second field has female student's score cards.

Write a method which calls the getScoreCardByGender method and gives the result which has more than 50%.

Write a method to find out similar percentage between the two groups (male, female). for example Geetika -75, Kunal -75

Write a method fo find out the percentage that girls group has scored but no same percentage has got in the boys group. e.g. ( Geetika -75, Neha - 73, charmy - 72) - (Kunal -75, Anmol - 73, Nitin - 71) = Charmy-72



*/
object Gender extends Enumeration{
        type Gender =Value
	val MALE,FEMALE = Value
}

 import Gender._
case class ScoreCard(studentId: Long, marks: Map[Long, Double], percentage: Double)
case class Student (id: Long, name:String , gen :Gender)
case class Marks(subjectid: Int, studentid : Long, marksobtained: Int)

class StudentName(sL : List[Student], mL : List[Marks]){


        def generatedScoreCard (currentStudent:Student):ScoreCard= { 


                  val mapping = generateIdMarksMap(currentStudent)
                  ScoreCard(currentStudent.id.toLong,mapping,calculateIt(mapping).toDouble)
 


        }

        def generateIdMarksMap(currentStudent:Student):Map[Long,Double]={

                     ( (for{
			iterator <- 0 to mL.size-1         
		        }yield if(mL(iterator).studentid == currentStudent.id)
                         mL(iterator).subjectid.toLong -> mL(iterator).marksobtained.toDouble
                         else 
                         -1.toLong -> -1.toDouble).filter{_ != -1.toLong -> -1.toDouble} ).toMap

        }  
	
	def calculateIt(list:Map[Long, Double]):Double = { 
 

             val li = (for {iterator <- 1 to 5}yield list(iterator).toInt).toList 
             val total:Double = li.foldLeft(0)(_ + _) 
             (total*100)/500
         }




	def getScoreCardByGender: (List[ScoreCard], List[ScoreCard]) ={                                        

			val result = sL map( x=> generatedScoreCard(x))
			val cards  =  (for{
				                       iterator <- 0 to sL.size-1
		  
				              }yield if(isMale(result(iterator).studentId)) result(iterator) 
                                                      else ScoreCard(0,Map(0.toLong->0.toDouble),0)).toList 

		       val males =  cards.filter{_ != ScoreCard(0,Map(0.toLong->0.toDouble),0) }

		       val females =  males.diff(result) ::: result.diff(males)

		        (males,females)
		     
		       
			    	
        }

	def print50percentagePeople:List[ScoreCard]={

          val maleFemaleTupple  =  getScoreCardByGender
          val mixed = maleFemaleTupple._1 ::: maleFemaleTupple._2
          val moreThen50 = (for{
		                iterator <- 0 to mixed.size-1
		  
	                     }yield if(mixed(iterator).percentage > 50) mixed(iterator) else ScoreCard(0,Map(0.toLong->0.toDouble),0) ).toList 
         println(s"\n\n=================== Score above 50% ===============\n")
          moreThen50.filter{ _ != ScoreCard(0,Map(0.toLong->0.toDouble),0)}
 
        }
        

        def isMale(thisid:Long):Boolean =
        { 
               
              val a = for{ iterator <- 0 to sL.size-1
  
                              }yield if(sL(iterator).id == thisid)sL(iterator).gen else FEMALE
              if(a.filter{ _ != FEMALE} == Vector(MALE)) true else false  

        } 

       def similarPercent : List[Any] = {
		val seq = nameToPercentMapping
		val male =   seq._1.flatten
                val female = seq._2.flatten

                
                val b=   male map(x =>

                                  (for{
                                         iterator <- 0 to female.length-1
                                     } yield if(female(iterator)._2 == x._2) List(female(iterator),x) else List(Nil,Nil)).toList

                              )


                   b.flatten.flatten.filter{ _ != List()}
    }
	

	def nameToPercentMapping : (List[Map[String, Double]],List[Map[String, Double]]) ={

		val scores = getScoreCardByGender
                val malescore = scores._1
		val femalescore = scores._2
                (support(malescore),support(femalescore))
	}

	def support(l1 : List[ScoreCard]) : List[Map[String, Double]] ={

		
		val pair = (l1 map(x => 

 				for{
					iterator <- 0 to sL.length-1
				   }yield if(x.studentId == sL(iterator).id) Map(sL(iterator).name -> x.percentage) else Map("" -> 0.0))).toList
			                     
                     
		pair.flatten.filter{ _ != Map("" -> 0.0)}
		
		}



}



object StudentGender extends App{

  import Gender._
  val male = MALE
  val female = FEMALE
  val studentList = List(Student(1,"sonu",male),Student(2,"Shubhra", female),Student(3,"bhavya", male),Student(4,"Shivangi",female),Student(5,"Ankit", male))


  val marksList = List(Marks(1,1,95),Marks(2,1,96),Marks(3,1,98),Marks(4,1,96),Marks(5,1,96),Marks(1,2,95),Marks(2,2,96),Marks(3,2,98),Marks(4,2,96),Marks(5,2,96),Marks(1,3,56),Marks(2,3,34),Marks(3,3,45),Marks(4,3,32),Marks(5,3,92),Marks(1,4,44),Marks(2,4,23),Marks(3,4,55),Marks(4,4,77),Marks(5,4,44),Marks(1,5,43),Marks(2,5,22),Marks(3,5,54),Marks(4,5,76),Marks(5,5,45))

  val studentobj = new StudentName(studentList,marksList)
  val list = studentobj.print50percentagePeople
  list.foreach(println) 
  println(s"\n\n=================== male-female having similar % ===============\n") 
  println(studentobj.similarPercent)
  println("\n__________________________END________________________\n\n")
}


//if(a->2)sl.groupby("a") = list

//traverse list be id by sending current student one by one

//return list(scorecard,scorecard)

//search by name
