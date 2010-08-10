package wsd.util;

/**
 * DictionaryEntry.java
 *
 * Dictionary Entry class: 
 * <p>
 * Organization of dictionary one record:
 * <pre>
 *  +------------------------+--------------------+-------------------+
 *  | term                   | number of postings |     address       |
 *  +------------------------+--------------------+-------------------+
 *  |<---- term length ----->|<---- 4 bytes ----->|<---- 4 bytes ---->|
 *  |<-------------------------- record length ---------------------->|
 * <pre>
 *  Term Length, # of postings And addr are the same for all records in a partition.
 * <p>
 * Created: Mon Aug 27 13:07:37 2001
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version $Id: DictionaryEntry.java,v 1.2 2006/09/20 21:09:43 wrogers Exp $
 */

public class DictionaryEntry
{
  String term;
  int numberOfPostings;
  int address;
  
  public DictionaryEntry (String term, int numberOfPostings, int address)
  {
    this.term = term;
    this.numberOfPostings = numberOfPostings;
    this.address = address;
  }
  
  public String getTerm()
  {
    return this.term;
  }

  public int getNumberOfPostings()
  {
    return this.numberOfPostings;
  }
  public int getAddress()
  {
    return this.address;
  }
}// DictionaryEntry



