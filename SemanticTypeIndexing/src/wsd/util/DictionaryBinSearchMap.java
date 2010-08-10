package wsd.util;
import java.io.*;

/**
 * Dictionary Binary Search Map.
 * <p>s

 * Organization of one dictionary record:
 * <pre>
 *  +------------------------+--------------------+---------------------+
 *  | term                   | number of postings | address of postings |
 *  +------------------------+--------------------+---------------------+
 *  |<---- term length ----->|<---- 4 bytes ----->|<----- 4 bytes ----->|
 *  |<--------------------------- record length ----------------------->|
 * </pre>
 * Term Length, # of postings length and address length are the same
 * for all records in a partition.
 * </p>
 * Created: Wed Jul 25 09:09:18 2001
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version $Id: DictionaryBinSearchMap.java,v 1.1 2006/09/25 18:33:07 wrogers Exp $
 */

public class DictionaryBinSearchMap implements BinSearchMap, Serializable {
 
  /** length of integer in bytes */
  public static final int DATALENGTH = 4; /* is this right? */
  /** data output stream for writing map. */
  private transient DataOutputStream mapWriter;
  /** random access file for reading map. */
  private transient RandomAccessFile mapRAFile;
  /** number of records in this map. */
  int numberOfRecords = 0;
  /** term length of all terms in this map. */
  private int termLength = 0;
  /** filename of map */
  String filename;

  /**
   * Instantiate a new or existing binary search map with single integers for data.
   * @param mapFilename filename of map
   * @param mode        file mode to use: utils.DictionaryBinSearchMap.WRITE to open map for writing, and
   *                    utils.DictionaryBinSearchMap.READ to open map for reading.
   */
  public DictionaryBinSearchMap ( String mapFilename, int mode )
    throws FileNotFoundException
  {
    if ( mode == WRITE ) {
      this.mapWriter = 
	new DataOutputStream ( new BufferedOutputStream
			       ( new FileOutputStream ( mapFilename )));
    } else {
      this.mapRAFile = new RandomAccessFile ( mapFilename, "r");
    }
    this.filename = mapFilename;
  }

  /**
   * Write an entry into map
   * @param term Term. 
   * @param numOfPostings Number of postings assoicated with term.
   * @param data Address of postings assoicated with term.
   */
  public void writeEntry(String term, int numOfPostings, int data)
    throws IOException
  {
    // write dictionary entry
    this.mapWriter.writeBytes(term);
    this.mapWriter.writeInt(numOfPostings);
    this.mapWriter.writeInt(data);
    this.numberOfRecords++;
    this.termLength = term.length();
  }

  /**
   * get data entry for term 
   * @param term term 
   * @return dictionary entry associated with term
   */
  public DictionaryEntry get(String term)
    throws IOException
  {
    if (this.mapRAFile == null ) {
       this.mapRAFile = new RandomAccessFile ( this.filename, "r");
    }
    return DiskBinarySearch.dictionaryBinarySearch(this.mapRAFile, 
					    term, term.length(), this.numberOfRecords);
  }

  /** 
   * @return get number of records in map
   */
  public int getNumberOfRecords()
  {
    return this.numberOfRecords;
  }

  /**
   * @return get length of data in each record
   */
  public int getDataLength()
  {
    return this.DATALENGTH;
  }

  /** close resources used by this map. */
  public void close()
    throws IOException
  {
    if (this.mapRAFile != null ) {
      this.mapRAFile.close();
    }
    if (this.mapWriter != null ) {
      this.mapWriter.close();
    }
  }

}// DictionaryBinSearchMap
