/*
 * SocialMediaTopicObjects.scala 
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

import com.cra.figaro.language.Universe
import com.cra.figaro.language.Flip
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.ElementCollection
import com.cra.figaro.language.Apply
import com.cra.figaro.util.memo
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

object SocialMediaTopicObjects {
  class Topic() extends ElementCollection {
    val hot = Flip(0.1)("hot", this)
  }

  val sports = new Topic()
  val politics = new Topic()

  class Person() {
    val interest = Uniform(sports, politics)
  }

  class Post(val poster: Person) extends ElementCollection {
    val topic = If(Flip(0.9), poster.interest, Uniform(sports, politics))("topic", this)
  }

  class Connection(person1: Person, person2: Person) {
    val connectionType = Uniform("acquaintance", "close friend", "family")
  }
  def generateConnection(pair: (Person, Person)) = new Connection(pair._1, pair._2)
  val connection = memo(generateConnection _)

  class Comment(val post: Post, val commenter: Person) {
    val isHot = post.get[Boolean]("topic.hot")
    val appropriateComment =
      Apply(post.topic, commenter.interest, isHot, (t1: Topic, t2: Topic, b: Boolean) => (t1 == t2) || b)
    val pair = ^^(appropriateComment, connection(post.poster, commenter).connectionType)
    def constraint(pair: (Boolean, String)) = {
      if (pair._1) 1.0
      else if (pair._2 == "family") 0.8
      else if (pair._2 == "close friend") 0.5
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

    post1.topic.observe(politics)

    println("Prior probabilities")
    println("Probability Amy's interest is politics = " + VariableElimination.probability(amy.interest, politics))
    println("Probability post 2's topic is sports = " + VariableElimination.probability(post2.topic, sports))
    println("Probability post 3's topic is sports = " + VariableElimination.probability(post3.topic, sports))
    println("Probability Amy is Brian's family = " + VariableElimination.probability(connection(brian, amy).connectionType, "family"))

    sports.hot.observe(true)

    println("\nAfter observing that sports is hot")
    println("Probability Amy's interest is politics = " + VariableElimination.probability(amy.interest, politics))
    println("Probability post 2's topic is sports = " + VariableElimination.probability(post2.topic, sports))
    println("Probability post 3's topic is sports = " + VariableElimination.probability(post3.topic, sports))
    println("Probability Amy is Brian's family = " + VariableElimination.probability(connection(brian, amy).connectionType, "family"))
 }
}

class SocialMediaTopicObjectsTest extends WordSpec with Matchers {
  Universe.createNew()
    
  class Topic() extends ElementCollection {
    val hot = Flip(0.1)("hot", this)
  }

  val sports = new Topic()
  val politics = new Topic()

  class Person() {
    val interest = Uniform(sports, politics)
  }

  class Post(val poster: Person) extends ElementCollection {
    val topic = If(Flip(0.9), poster.interest, Uniform(sports, politics))("topic", this)
  }

  class Connection(person1: Person, person2: Person) {
    val connectionType = Uniform("acquaintance", "close friend", "family")
  }
  def generateConnection(pair: (Person, Person)) = new Connection(pair._1, pair._2)
  val connection = memo(generateConnection _)

  class Comment(val post: Post, val commenter: Person) {
    val isHot = post.get[Boolean]("topic.hot")
    val appropriateComment =
      Apply(post.topic, commenter.interest, isHot, (t1: Topic, t2: Topic, b: Boolean) => (t1 == t2) || b)
    val pair = ^^(appropriateComment, connection(post.poster, commenter).connectionType)
    def constraint(pair: (Boolean, String)) = {
      if (pair._1) 1.0
      else if (pair._2 == "family") 0.8
      else if (pair._2 == "close friend") 0.5
      else 0.1
    }
    pair.addConstraint(constraint _)
  }
  
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

  post1.topic.observe(politics)
    
  "Social Media Topic Objects for prior probabilities" should {
    "produce a probability Amy's interest is politics = 0.9656697011762803" taggedAs (BookExample) in {
      VariableElimination.probability(amy.interest, politics) should be(0.9656697011762803)
    }
    "produce a probability post 2's topic is sports = 0.24190044937009827" taggedAs (BookExample) in {
      VariableElimination.probability(post2.topic, sports) should be(0.24190044937009827)
    }
    "produce a probability post 3's topic is sports = 0.06958146174469142" taggedAs (BookExample) in {
      VariableElimination.probability(post3.topic, sports) should be(0.06958146174469142)
    }
    "produce a probability Brian is Amy's family = 0.3791735849751334" taggedAs (BookExample) in {
      VariableElimination.probability(connection(brian, amy).connectionType, "family") should be(0.3791735849751334)
    }
  }

  sports.hot.observe(true)

  "Social Media Topic Objects after observing that sports is hot" should {
    "produce a probability Amy's interest is politics = 0.9609178093049061" taggedAs (BookExample) in {
      VariableElimination.probability(amy.interest, politics) should be(0.9609178093049061)
    }
    "produce a probability post 2's topic is sports = 0.3729396845525878" taggedAs (BookExample) in {
      VariableElimination.probability(post2.topic, sports) should be(0.3729396845525878)
    }
    "produce a probability post 3's topic is sports = 0.09005551240389947" taggedAs (BookExample) in {
      VariableElimination.probability(post3.topic, sports) should be(0.09005551240389947)
    }
    "produce a probability Amy is Brian's family = 0.33670840928905443" taggedAs (BookExample) in {
      VariableElimination.probability(connection(brian, amy).connectionType, "family") should be(0.33670840928905443)
    }
  }
}
