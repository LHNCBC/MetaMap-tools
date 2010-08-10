package wsd.util;
import java.io.*;
import java.util.*;

class TemporaryIntArrayPostingsPool {
  String postingsFilename = "tpost";
  transient private RandomAccessFile postingsRAF = null;
  static final int BUFFER_SIZE = 1500;
  byte[] buffer = new byte[BUFFER_SIZE];
  int lastIndex = 0;
  /** string containing known punctuation */
  public static final String PUNCTUATION = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";
  /** string containing known whitespace */
  public static final String WHITESPACE = " \t\n\r\f";


  public TemporaryIntArrayPostingsPool (String aPostingsFilename, String mode)
  {
    this.postingsFilename = aPostingsFilename;
    try {
      this.postingsRAF =  new RandomAccessFile(this.postingsFilename, mode);
    } catch (FileNotFoundException exception) {
      exception.printStackTrace(System.err);
    }
  }

  /**
   * @param posting integer vector data to post in ASCII String format
   * @return address of posting
   */
  public int add(String posting)
  {
    StringTokenizer st = new StringTokenizer(posting, this.WHITESPACE);
    int[] termVector = new int[st.countTokens() - 1];
    int i = 0;
    st.nextToken(); // skip first token, the key
    while (st.hasMoreTokens()) {
      String coeff = st.nextToken();
      // System.out.println("coeff: " + coeff);
      termVector[i] = Integer.parseInt(coeff);
      i++;
    }
    return this.add(termVector);
  }

  /**
   * @param posting integer vector data to post 
   * @return address of posting
   */
  public int add(int[] posting)
  {
    int address = -1;
    try {
      this.postingsRAF.writeInt(posting.length);
      for (int i = 0; i<posting.length; i++)
        this.postingsRAF.writeInt(posting[i]);
      address = this.lastIndex;
      this.lastIndex = this.lastIndex + (posting.length*4) + 4;
    } catch (Exception exception) {
      System.err.println("add(): exception: " + exception.getMessage());
    }
    return address;
  }

  public int[] get(int address)
  {
    try {
      this.postingsRAF.seek(address);
      int length = this.postingsRAF.readInt();
      int[] posting = new int[length];
      for (int i = 0; i<length; i++)
        posting[i] = this.postingsRAF.readInt();
      return posting;
    } catch (Exception exception) {
      System.err.println("TemporaryIntArrayPostingPool:get(): exception: " + exception.getMessage());
    }
    return null;
  }


  public void close()
  { 
    try {
      this.postingsRAF.close();
    } catch (Exception exception) {
      System.err.println("get(): exception: " + exception.getMessage());
    }
  }
}
