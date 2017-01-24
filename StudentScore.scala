/*
Q1. Now, I require a case class named ScoreCard having fields (studentId: Long, marks: Map[Long, Float], percentage: Float).

Write a method which takes no parameter and generates a Map with key student name and value as ScoreCard. As there can be more than one student with same name, the logic we have to follow is that, if two or more student has same name the key shold be the name of the student and the values (ScoreCard s) should be in a List, otherwise the key should be the student name and value should be the case class ScoreCard. e.g. Map should be Map[String, AnyRef]. 

Write a method which takes input as student name and print the score cards. If it finds one or more than one score card  print all of them other wise print "No data found". The print should be in increasing order of the student id.

*/
case class ScoreCard(studentId: Long, marks: Map[Long, Double], percentage: Double)
case class Student(id: Long, name:String ,gender:Char)
case class Marks(subjectid: Int, studentid : Long, marksobtained: Int)

class ProcessStudent(sL : List[Student], mL : List[Marks]){


        def printscore :Map[String,AnyRef] = {
		(sL map( x => x.name -> generatedScoreCard(x,ListOfDuplicateNames))).toMap
		 
	}

        def generatedScoreCard (currentStudent:Student,duplicates:List[String]):AnyRef= { 

            if(duplicates.contains(currentStudent.name))
            {
                  val duplicateDetails = sL.groupBy( _.name == currentStudent.name )
                  val duplicates = duplicateDetails(true)

                 (for{
                     iterator <- 0 to duplicates.length-1
                     }yield ScoreCard(duplicates(iterator).id.toLong,generateIdMarksMap(duplicates(iterator)),
                            calculateIt(generateIdMarksMap(duplicates(iterator))).toDouble)).toList

            
 

            }
            else{
                  val mapping = generateIdMarksMap(currentStudent)
                  ScoreCard(currentStudent.id.toLong,mapping,calculateIt(mapping).toDouble)
            }


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

         def ListOfDuplicateNames:List[String]={

         val raw = sL map (x=>
                               (for{
                                           iterator <- 0 to sL.length-1
                               }yield if(x.name == sL(iterator).name && x.id != sL(iterator).id) x.name else "").toList.filter{ _ != ""}
 
                         )
              val duplicateFound =  raw.toSet.toList.filter{ _ != List()} 
              duplicateFound.toSet.flatten.toList
         }

	def searchByName(searchString:String,Data:Map[String,AnyRef]):AnyRef = { 
       
         println(s"\n\n============ Avaliable scorecard for $searchString ==========\n")

             val mykeys = Data.keySet.toList
             if(mykeys.contains(searchString))
             {

                 Data(searchString)
             
             }
            else List("No data found")         
          
            
        }

}



object StudentScore extends App{

  val studentList = List(Student(1,"sonu",'m'),Student(2,"Shivangi",'f'),Student(3,"bhavya",'m'),Student(4,"Shivangi",'f'),Student(5,"sonu",'m'))


  val marksList = List(Marks(1,1,95),Marks(2,1,96),Marks(3,1,98),Marks(4,1,96),Marks(5,1,96),Marks(1,2,56),Marks(2,2,10),Marks(3,2,20),Marks(4,2,0),Marks(5,2,53),Marks(1,3,56),Marks(2,3,34),Marks(3,3,45),Marks(4,3,32),Marks(5,3,92),Marks(1,4,44),Marks(2,4,23),Marks(3,4,55),Marks(4,4,77),Marks(5,4,44),Marks(1,5,43),Marks(2,5,22),Marks(3,5,54),Marks(4,5,76),Marks(5,5,45))

  val studentobj = new ProcessStudent(studentList,marksList)
  val l =studentobj.printscore
  val searchResult = studentobj.searchByName("bhavya",l)
  println(searchResult)

  println("\n__________________________END________________________\n\n")
}


//if(a->2)sl.groupby("a") = list

//traverse list be id by sending current student one by one

//return list(scorecard,scorecard)

//search by name
