package neophyte.part10

object Part10 {
  val me = EMailAddress("Roland", "roland@tritsch.org")

  val goodAddresses = Set(
    me,
    EMailAddress("Daniel", "dwestheide@gmail.com"),
    EMailAddress("Joe", "joe@bar.com"),
    EMailAddress("Jane", "jane@bar.com")
  )

  val badAddresses = Set(
    EMailAddress("Foo", "foo@bar.com")
  )

  val addressBook = goodAddresses.union(badAddresses)

  val inbox = Set(
    EMail(
      from = me,
      to = EMailAddress("Daniel", "dwestheide@gmail.com"),
      subject = "Awesome articles",
      body = "Hi Daniel, thanks a lot for this. - RT"
    ),
    EMail(
      EMailAddress("Daniel", "dwestheide@gmail.com"),
      me,
      "RE: Awesome articles",
      "Hi Roland, thanks and you are welcome. Regards ... Daniel"
    ),
    EMail(
      EMailAddress("Foo", "foo@bar.com"),
      me,
      "I am Nigerian Prince",
      "Hi Roland, I need to transfer USD 10M to your back account. Can you help me? - Prince UuiyebdaS"
    )
  )

  def main(args: Array[String]): Unit = {
    require(args.size == 0, "Usage: neophyte.Part10")

    println(filterSpam(inbox, sendByOneOf(badAddresses)))
    println(filterSpam(inbox, notSendByAnyOf(badAddresses)))
    println(filterSpam(inbox, minimumSize(80)))
    println(filterSpam(inbox, maximumSize(50)))
    println(filterSpam(inbox, minimumSize2(80)))
    assert(filterSpam(inbox, minimumSize(80)).sameElements(filterSpam(inbox, minimumSize2(80))))
    println(filterSpam(inbox, maximumSize2(50)))
    assert(filterSpam(inbox, maximumSize(50)).sameElements(filterSpam(inbox, maximumSize2(50))))
    println(filterSpam(inbox, notSendByAnyOf2(badAddresses)))
    assert(filterSpam(inbox, notSendByAnyOf(badAddresses)).sameElements(filterSpam(inbox, notSendByAnyOf2(badAddresses))))
    println(filterSpam(inbox, all(notSendByAnyOf(badAddresses), maximumSize(50))))
    println(filterSpam(inbox, all(notSendByAnyOf(badAddresses), minimumSize(80))))
    println(filterSpam(inbox, any(notSendByAnyOf(badAddresses), minimumSize(80))))
    println(filterSpam(inbox, none(notSendByAnyOf(badAddresses), minimumSize(80))))
    println(inbox.map(Function.chain(Seq(
      addMissingSubject,
      checkSpelling,
      removeInapprobriateLanguage,
      addAdvertisementToFooter
    ))))
  }

  type SpamFilter = EMail => Boolean

  def filterSpam(inComing: Set[EMail], filter: SpamFilter): Set[EMail] = inComing.filter(filter)

  val sendByOneOf: Set[EMailAddress] => SpamFilter = {
    senders => email => senders.contains(email.from)
  }

  val notSendByAnyOf: Set[EMailAddress] => SpamFilter = {
    senders => email => !senders.contains(email.from)
  }

  val minimumSize: Int => SpamFilter = {
    n => EMail => EMail.body.size >= n
  }

  val maximumSize: Int => SpamFilter = {
    n => EMail => EMail.body.size <= n
  }

  type SizeChecker = Int => Boolean

  val sizeConstraint: SizeChecker => SpamFilter = {
    f => email => f(email.body.size)
  }

  val minimumSize2: Int => SpamFilter = {
    n => sizeConstraint(_ >= n)
  }

  val maximumSize2: Int => SpamFilter = {
    n => sizeConstraint(_ <= n)
  }

  def complement[A](predicate: A => Boolean): A => Boolean = {
    (a: A) => !predicate(a)
  }

  val notSendByAnyOf2: Set[EMailAddress] => SpamFilter = {
    sendByOneOf.andThen(g => complement(g))
  }

  def any[A](predicates: (A => Boolean)*): A => Boolean = {
    a => predicates.exists(p => p(a))
  }

  def none[A](predicates: (A => Boolean)*): A => Boolean = {
    complement(any(predicates: _*))
  }

  def all[A](predicates: (A => Boolean)*): A => Boolean = {
    none(predicates.view.map(complement(_)): _*)
  }

  val addMissingSubject: EMail => EMail = {
    (email: EMail) => if (email.subject.isEmpty) email.copy(subject = "No subject") else email
  }

  val checkSpelling: EMail => EMail = {
    (email: EMail) => email.copy(body = email.body.replaceAll("you are", "you're"))
  }

  val removeInapprobriateLanguage: EMail => EMail = {
    (email: EMail) => email.copy(body = email.body.replaceAll("dynamic typing", "***PEEEEP***"))
  }

  val footer = "---\nroland@tritsch.org\nDublin, Ireland\n---\n\n"
  val addAdvertisementToFooter: EMail => EMail = {
    (email: EMail) => email.copy(body = s"${email.body}\n${footer}")
  }
}

case class EMailAddress(name: String, uri: String)
case class EMail(from: EMailAddress, to: EMailAddress, subject: String, body: String)