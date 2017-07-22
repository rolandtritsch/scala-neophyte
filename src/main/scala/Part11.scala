package neophyte.part11

import neophyte.part10.Part10
import neophyte.part10.EMail
import neophyte.part10.EMailAddress
import neophyte.part10.Part10.SpamFilter

object Part11 {
  def main(args: Array[String]): Unit = {
    require(args.size == 0, "Usage: neophyte.Part11")

    println(Part10.filterSpam(Part10.inbox, min20))
    println(Part10.filterSpam(Part10.inbox, max20))
    println(Part10.filterSpam(Part10.inbox, min30))
    println(Part10.filterSpam(Part10.inbox, max30))
    println(Part10.filterSpam(Part10.inbox, min40))
    println(Part10.filterSpam(Part10.inbox, max40))
    println(Part10.filterSpam(Part10.inbox, min50))
    println(Part10.filterSpam(Part10.inbox, max50))

    println(MailboxServiceWithMockDeps.newMails(Part10.me))
    println(MyMailboxService.newMails(Part10.me))
  }

  type IntPairPred = (Int, Int) => Boolean
  def sizeConstraint(pred: IntPairPred, n: Int, email: EMail): Boolean = {
    pred(email.body.size, n)
  }

  val gt: IntPairPred = _ > _
  val ge: IntPairPred = _ >= _
  val lt: IntPairPred = _ < _
  val le: IntPairPred = _ <= _
  val eq: IntPairPred = _ == _

  val minimumSize: (Int, EMail) => Boolean = {
    sizeConstraint(ge, _: Int, _: EMail)
  }

  val maximumSize: (Int, EMail) => Boolean = {
    sizeConstraint(lt, _: Int, _: EMail)
  }

  val constr20: (IntPairPred, EMail) => Boolean = {
    sizeConstraint(_: IntPairPred, 20, _: EMail)
  }

  val constr30: (IntPairPred, EMail) => Boolean = {
    sizeConstraint(_: IntPairPred, 30, _: EMail)
  }

  val sizeConstraintFn: (IntPairPred, Int, EMail) => Boolean = {
    sizeConstraint _
  }

  val min20: Part10.SpamFilter = minimumSize(20, _: EMail)
  val max20: Part10.SpamFilter = maximumSize(20, _: EMail)
  val min30: Part10.SpamFilter = constr30(ge, _: EMail)
  val max30: Part10.SpamFilter = constr30(lt, _: EMail)

  def sizeConstraint2(pred: IntPairPred)(n: Int)(eMail: EMail): Boolean = {
    pred(eMail.body.size, n)
  }

  val sizeConstraintFn2: IntPairPred => Int => EMail => Boolean = sizeConstraint2 _

  val min40: EMail => Boolean = sizeConstraintFn2(ge)(40)
  val max40: EMail => Boolean = sizeConstraintFn2(lt)(40)

  val sizeConstraintFn3: IntPairPred => Int => EMail => Boolean = sizeConstraintFn.curried

  val min50: EMail => Boolean = sizeConstraintFn2(ge)(50)
  val max50: EMail => Boolean = sizeConstraintFn2(lt)(50)
}

trait EMailRepository {
  def getMails(user: EMailAddress, unread: Boolean): Set[EMail]
}

trait FilterRepository {
  def getSpamFilter(user: EMailAddress): Part10.SpamFilter
}

trait MailboxService {
  def getNewMails(emailRepo: EMailRepository)(filterRepo: FilterRepository)(user: EMailAddress): Set[EMail] = {
    emailRepo.getMails(user, true).filter(filterRepo.getSpamFilter(user))
  }
  val newMails: EMailAddress => Set[EMail]
}

object MockEMailRepository extends EMailRepository {
  override def getMails(user: EMailAddress, unread: Boolean): Set[EMail] = Set.empty
}

object MockFilterRepository extends FilterRepository {
  override def getSpamFilter(user: EMailAddress): Part10.SpamFilter = _ => true
}

object MailboxServiceWithMockDeps extends MailboxService {
  override val newMails: (EMailAddress) => Set[EMail] = {
    getNewMails(MockEMailRepository)(MockFilterRepository) _
  }
}

object MyEMailRepository extends EMailRepository {
  override def getMails(user: EMailAddress, unread: Boolean): Set[EMail] = Part10.inbox
}

object MyFilterRepository extends FilterRepository {
  override def getSpamFilter(user: EMailAddress): SpamFilter = Part11.max50
}

object MyMailboxService extends MailboxService {
  override val newMails: (EMailAddress) => Set[EMail] = {
    getNewMails(MyEMailRepository)(MyFilterRepository) _
  }
}