package wsd.util;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;


public class IntArrayBinSearchPool
{
  /** pool of DataBinSearchMaps */
  Map pool = new HashMap(20);

  String indexname;
  String indexDirectoryPath;

  public IntArrayBinSearchPool(String indexDirectoryPath, String indexname) {
    this.indexDirectoryPath = indexDirectoryPath;
    this.indexname = indexname;
  }

  public String getPartitionId(String term)
  {
    String keyLength = Integer.toString(term.length());
    return this.indexname+keyLength;
  }

  public DataBinSearchMap getDataBinSearchMap(String partitionId)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    return DataBinSearchMap.getInstance(this.indexDirectoryPath + File.separator + "partition_" + partitionId);
  }

  public int[] get(String term)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    DataBinSearchMap dbsm = null;
    String partitionId = getPartitionId(term);
    if (pool.containsKey(partitionId)) {
      dbsm = (DataBinSearchMap)pool.get(partitionId);
    } else {
      dbsm = getDataBinSearchMap(partitionId);
      synchronized (pool) {
        pool.put(partitionId,dbsm);
      }
    }
    synchronized (dbsm)
      {
        return dbsm.getIntArray(term);
      }
  }
  
  public int[] get(String term, int[] intArray)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    DataBinSearchMap dbsm = null;
    String partitionId = getPartitionId(term);
    if (pool.containsKey(partitionId)) {
      dbsm = (DataBinSearchMap)pool.get(partitionId);
    } else {
      dbsm = getDataBinSearchMap(partitionId);
      synchronized (pool) {
        pool.put(partitionId,dbsm);
      }
    }
    if (dbsm == null) 
      return null;
    else 
      synchronized (dbsm)
        {
          return dbsm.getIntArrayRef(term, intArray);
        }
  }

  public void close()
    throws IOException
  {
    Iterator poolIter = pool.values().iterator();
    while (poolIter.hasNext()) {
      ((DataBinSearchMap)poolIter.next()).close();
    }
  }
}

