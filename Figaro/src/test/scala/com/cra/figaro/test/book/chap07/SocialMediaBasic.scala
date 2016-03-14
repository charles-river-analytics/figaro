/*
 * SocialMediaBasic.scala 
 * Book example unit test.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com), Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 26, 2016
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.book.chap07

import com.cra.figaro.language.{Flip, Universe}
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.util.memo
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

object SocialMediaBasic {
  class Person() {
    val interest = Uniform("sports", "politics")
  }

  class Post(val poster: Person) {
    val topic = If(Flip(0.9), poster.interest, Uniform("sports", "politics"))
  }

  class Connection(person1: Person, person2: Person) {
    val connectionType = Uniform("acquaintance", "close friend", "family")
  }
  def generateConnection(pair: (Person, Person)) = new Connection(pair._1, pair._2)
  val connection = memo(generateConnection _)

  class Comment(val post: Post, val commenter: Person) {
    val topicMatch = post.topic === commenter.interest
    val pair = ^^(topicMatch, connection(post.poster, commenter).connectionType)
    def constraint(pair: (Boolean, String)) = {
      val (topicMatch, connectionType) = pair
      if (topicMatch) 1.0
      else if (connectionType == "family") 0.8
      else if (connectionType == "close friend") 0.5
      else 0.1
    }
    pair.addConstraint(constraint _)
  }

  def main(args: Array[String]) {
    val amy = new Person()
    val brian = new Person()
    val cheryl = new Person()

    val post1 = new Post(amy)
    val post2 = new Post(brian)
    val post3 = new Post(amy)

    val comment1 = new Comment(post1, brian)
    val comment2 = new Comment(post1, cheryl)
    val comment3 = new Comment(post2, amy)
    val comment4 = new Comment(post3, cheryl)

    post1.topic.observe("politics")
    post2.topic.observe("sports")
    post3.topic.observe("politics")

    println("Probability Amy's interest is politics = " + VariableElimination.probability(amy.interest, "politics"))
    println("Probability Brian's interest is politics = " + VariableElimination.probability(brian.interest, "politics"))
    println("Probability Cheryl's interest is politics = " + VariableElimination.probability(cheryl.interest, "politics"))
    println("Probability Brian is Amy's family = " + VariableElimination.probability(connection(amy, brian).connectionType, "family"))
    println("Probability Cheryl is Amy's family = " + VariableElimination.probability(connection(amy, cheryl).connectionType, "family"))
    println("Probability Cheryl is Brian's family = " + VariableElimination.probability(connection(brian, cheryl).connectionType, "family"))
  }
}

class SocialMediaBasicTest extends WordSpec with Matchers {
  Universe.createNew()
  val connection = memo(SocialMediaBasic.generateConnection _)
  
  val amy = new SocialMediaBasic.Person()
  val brian = new SocialMediaBasic.Person()
  val cheryl = new SocialMediaBasic.Person()

  val post1 = new SocialMediaBasic.Post(amy)
  val post2 = new SocialMediaBasic.Post(brian)
  val post3 = new SocialMediaBasic.Post(amy)

  val comment1 = new SocialMediaBasic.Comment(post1, brian)
  val comment2 = new SocialMediaBasic.Comment(post1, cheryl)
  val comment3 = new SocialMediaBasic.Comment(post2, amy)
  val comment4 = new SocialMediaBasic.Comment(post3, cheryl)

  post1.topic.observe("politics")
  post2.topic.observe("sports")
  post3.topic.observe("politics")

  val amyPolitics = VariableElimination.probability(amy.interest, "politics")
  val brianPolitics = VariableElimination.probability(brian.interest, "politics")
  val cherylPolitics = VariableElimination.probability(cheryl.interest, "politics")
  val brianAmyFamily = VariableElimination.probability(connection(amy, brian).connectionType, "family")
  val cherylAmyFamily = VariableElimination.probability(connection(amy, cheryl).connectionType, "family")
  val cherylBrianFamily = VariableElimination.probability(connection(brian, cheryl).connectionType, "family")
  
  "Social Media Basic" should {
    "produce a probability Amy's interest is politics = 0.9940991345397325" taggedAs (BookExample) in {
      amyPolitics should be(0.9940991345397325)
    }
    "produce a probability Brian's interest is politics = 0.10135135135135133" taggedAs (BookExample) in {
      brianPolitics should be(0.10135135135135133)
    }
    "produce a probability Cheryl's interest is politics = 0.7692307692307692" taggedAs (BookExample) in {
      cherylPolitics should be(0.7692307692307692)
    }
    "produce a probability Brian is Amy's family = 0.33333333333333337" taggedAs (BookExample) in {
      brianAmyFamily should be(0.33333333333333337)
    }
    "produce a probability Cheryl is Amy's family = 0.3333333333333333" taggedAs (BookExample) in {
      cherylAmyFamily should be(0.3333333333333333)
    }
    "produce a probability Cheryl is Brian's family = 0.3333333333333333" taggedAs (BookExample) in {
      cherylBrianFamily should be(0.3333333333333333)
    }
  }
}
