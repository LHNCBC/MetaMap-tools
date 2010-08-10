package wsd.util;
import java.io.*;

/**
 * Signatures for Binary Search Map classes.
 *
 *
 * Created: Wed Jul 25 09:11:23 2001
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">"Willie Rogers</a>
 * @version $Id: BinSearchMap.java,v 1.1 2006/08/18 19:04:21 wrogers Exp $
 */

public interface BinSearchMap 
{
  /** open map for reading */
  public static final int READ = 0;
  /** open map for writing */
  public static final int WRITE = 1;

  /**
   * @return get number of records in map
   */
  int getNumberOfRecords();

  /**
   * @return get length of data in each record
   */
  int getDataLength();
 
  /** close resources used by this map. */
  void close() throws IOException;

} // BinSearchMap
