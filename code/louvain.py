from community import community_louvain
import networkx as nx
import pandas as pd
import os

def savePartitionsIntoCSV(df, source, target, weight):
    
    #convert directed to undirected
    df['key'] = df.apply(lambda row: tuple(sorted((row[source], row[target]))), axis=1)
    df = df.groupby('key')[weight].sum().reset_index()
    df['nodeA'] = df['key'].apply(lambda x: x[0])
    df['nodeB'] = df['key'].apply(lambda x: x[1])
    del df['key']
    
    #find partitions
    G = nx.Graph() #use nx.DiGraph() for directed networks.
    G = nx.from_pandas_edgelist(df, 'nodeA', 'nodeB', edge_attr=weight)
    
    partition = community_louvain.best_partition(G, weight=weight)
    modularity = community_louvain.modularity(partition, G, weight=weight)
    print("The modularity is {}".format(modularity))
    
    partition_df = pd.DataFrame.from_dict(partition, orient='index', columns=['partition']).reset_index()
    print("The number of partitions is {}".format(len(partition_df['partition'].unique())))
    
    partition_df.rename(columns={'index':'node'}, inplace=True)
    
    textlist = ["The modularity is {}".format(modularity), "The number of partitions is {}".format(len(partition_df['partition'].unique()))]

    return partition_df, textlist


def louvainForFilesInFolder(rootdir, flowfilephrase, source, target, weight):

    for subdir, dirs, files in os.walk(rootdir):
    	for file in files:
            if ((flowfilephrase in file) and (".csv" in file)):
                filename = file[-13:]
                df = pd.read_csv(os.path.join(subdir, file))
                partition, text = savePartitionsIntoCSV(df, source, target, weight)
                
                newpath = subdir+"/partition"
                if not os.path.exists(newpath):
                    os.makedirs(newpath)
                    
                partition.to_csv(newpath+"/partition_"+filename, index=False)
            
                with open(newpath+"/partition_summary_"+filename[:-4]+".txt",'w') as f:
                    f.write('\n'.join(text))
                    f.close()
