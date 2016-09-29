package com.emarsys.dojo

import org.scalatest.{Matchers, WordSpec}
import OrderJob._

class OrderJobSpec extends WordSpec with Matchers {

  "OrderJob" should {

    "return empty in case of empty input" in {
      ordered("") shouldEqual ""
    }

    "return single job in case of one element in input" in {
      ordered("a =>") shouldEqual "a"
    }

    "return jobs in case of sequence of jobs without dependency in input" in {
      ordered("a =>|b =>|c =>") shouldEqual "abc"
    }

    "return jobs in case of multiple jobs single dependency in input" in {
      ordered("a =>|b => c|c =>") shouldEqual "acb"
    }

    "return jobs in case of multiple jobs multiple dependency in input" in {
      ordered("a =>|b => c|c => f|d => a|e => b|f =>") shouldEqual "afcdbe"
    }

    "return jobs in case of multiple jobs self referencing dependency in input" in {
      ordered("a =>|b =>|c => c") shouldEqual "self dependency"
    }

    "return jobs in case of multiple jobs dependency chain in input" in {
      ordered("a => b|b => c|c => a") shouldEqual "circular dependency"
    }
  }
}
