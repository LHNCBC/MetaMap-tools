package wsd.util;
import java.io.*;

/**
 * DiskBinarySearch.java
 *
 *
 * Created: Wed Jul 25 11:15:59 2001
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version $Id: DiskBinarySearch.java,v 1.3 2006/09/20 21:06:19 wrogers Exp $
 */

public final class DiskBinarySearch extends Object
{

  /**
   *  Disk based binary search implementation
   *
   * @param bsfp       file pointer for binary search table
   * @param word       search word
   * @param wordlen    wordlength
   * @param numrecs    number of records in table
   * @param datalen    length of associated data

   * @return byte array containing binary data
   *          associated with search word or null if term not found.
   */
  public static byte[] binarySearch(RandomAccessFile bsfp, String word, int wordlen, int numrecs, int datalen)
    throws IOException
  {
    // d1 or i1 if double then bytelen is 8 else int of bytelen 4.
    int low = 0;
    int high = numrecs;
    int cond;
    int mid;
    byte[] wordbuf = new byte[wordlen];
    String tstword;
    byte[] data = new byte[datalen];

    synchronized(bsfp) 
      {
        while ( low < high )
          {
            mid = low + (high- low) / 2;
            bsfp.seek(mid * (wordlen+datalen));
            bsfp.read(wordbuf);
            tstword = new String(wordbuf);
            cond = word.compareTo(tstword);
            if (cond < 0) {
              high = mid;
            } else if (cond > 0) {
              low = mid + 1;
            } else {
              bsfp.read(data);
              return data;
            }
          }
      }
    return null;
  }

  /**
   *  Disk based binary search implementation
   *
   * @param bsfp       file pointer for binary search table
   * @param word       search word
   * @param wordlen    wordlength
   * @param numrecs    number of records in table
   * @return int containing address of posting, -1 if not found.
   */
  public static int intBinarySearch(RandomAccessFile bsfp, String word, int wordlen, int numrecs)
    throws IOException
  {
    // d1 or i1 if double then bytelen is 8 else int of bytelen 4.
    int datalen = 4;
    int low = 0;
    int high = numrecs;
    int cond;
    int mid;
    byte[] wordbuf = new byte[wordlen];
    String tstword;

    synchronized(bsfp) 
      {
	while ( low < high )
	  {
	    mid = low + (high- low) / 2;
	    bsfp.seek(mid * (wordlen+datalen));
	    bsfp.read(wordbuf);
	    tstword = new String(wordbuf);
	    cond = word.compareTo(tstword);
	    if (cond < 0) {
	      high = mid;
	    } else if (cond > 0) {
	      low = mid + 1;
	    } else {
	      return bsfp.readInt();
	    }
	  }
      }
    return -1;
  }

  /**
   *  Disk based binary search implementation
   *
   * @param bsfp       file pointer for binary search table
   * @param word       search word
   * @param wordlen    wordlength
   * @param numrecs    number of records in table
   * @return int containing address of posting, -1 if not found.
   */
  public static DictionaryEntry
    dictionaryBinarySearch(RandomAccessFile bsfp, String word, 
			   int wordlen, int numrecs)
    throws IOException
  {
    int datalen = 8; // postings (integer[4 bytes]) + address (integer[4 bytes])
    int low = 0;
    int high = numrecs;
    int cond;
    int mid;
    byte[] wordbuf = new byte[wordlen];
    String tstword;

    while ( low < high )
      {
	mid = low + (high- low) / 2;
	bsfp.seek(mid * (wordlen+datalen));
	bsfp.read(wordbuf);
	tstword = new String(wordbuf);
	// System.out.println("tstword: " + tstword + ", word: " + word);
	cond = word.compareTo(tstword);
	if (cond < 0) {
	  high = mid;
	} else if (cond > 0) {
	  low = mid + 1;
	} else {
	  int count = bsfp.readInt();
	  int address = bsfp.readInt();
	  return new DictionaryEntry(tstword, count, address);
	}
      }
    return null;
  }

  /**
   *  Disk based binary search implementation
   *
   * @param bsfp       file pointer for binary search table
   * @param word       search word
   * @param wordlen    wordlength
   * @param numrecs    number of records in table
   * @param datalen    length of associated data

   * @return integer array containing binary data
   *          associated with search word or null if term not found.
   */
  public static int[] intArrayBinarySearch(RandomAccessFile bsfp, String word, int wordlen, int numrecs, int datalen)
    throws IOException
  {
    // d1 or i1 if double then bytelen is 8 else int of bytelen 4.
    int low = 0;
    int high = numrecs;
    int cond;
    int mid;
    byte[] wordbuf = new byte[wordlen];
    String tstword;
    int[] data = new int[datalen];

    synchronized(bsfp) 
      {
        while ( low < high )
          {
            mid = low + (high- low) / 2;
            bsfp.seek(mid * (wordlen+(datalen*4)));
            bsfp.read(wordbuf);
            tstword = new String(wordbuf);
            cond = word.compareTo(tstword);
            if (cond < 0) {
              high = mid;
            } else if (cond > 0) {
              low = mid + 1;
            } else {
              for (int i = 0; i < datalen; i++) {
                data[i] = bsfp.readInt();
              }
              return data;
            }
          }
      }
    return null;
  }


  /**
   *  Disk based binary search implementation
   *
   * @param bsfp       file pointer for binary search table
   * @param word       search word
   * @param wordlen    wordlength
   * @param numrecs    number of records in table
   * @param datalen    length of associated data

   * @return integer array containing binary data
   *          associated with search word or null if term not found.
   */
  public static int[] intArrayBinarySearch(RandomAccessFile bsfp, String word, int wordlen, int numrecs, int datalen, int[] data)
    throws IOException
  {

    // d1 or i1 if double then bytelen is 8 else int of bytelen 4.
    int low = 0;
    int high = numrecs;
    int cond;
    int mid;
    byte[] wordbuf = new byte[wordlen];
    String tstword;

    if (data.length >= datalen) {
      synchronized(bsfp) 
        {
          while ( low < high )
            {
              mid = low + (high- low) / 2;
              bsfp.seek(mid * (wordlen+(datalen*4)));
              bsfp.read(wordbuf);
              tstword = new String(wordbuf);
              cond = word.compareTo(tstword);
              if (cond < 0) {
                high = mid;
              } else if (cond > 0) {
                low = mid + 1;
              } else {
                for (int i = 0; i < datalen; i++) {
                  data[i] = bsfp.readInt();
                }
                return data;
              }
            }
        }
    } else {
      System.out.println("Integer data array too small not modifying array and returning null!");
    }
    return null;
  }

} // DiskBinarySearch
