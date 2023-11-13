import igraph as ig
import leidenalg as la
import networkx as nx
import pandas as pd
import glob
import os

def leidenPartitions(df, source, target, weight):
    
    # build dataframe
    df['key'] = df.apply(lambda row: tuple(sorted((row[source], row[target]))), axis=1)
    df = df.groupby('key')[weight].sum().reset_index()
    df['nodeA'] = df['key'].apply(lambda x: x[0])
    df['nodeB'] = df['key'].apply(lambda x: x[1])
    del df['key']
    
    # define undirected igraph
    tuples = [tuple(x) for x in df[['nodeA', 'nodeB', weight]].values]
    G_igraph = ig.Graph.TupleList(tuples, edge_attrs = [weight])

    # Apply the Leiden algorithm
    partition = la.find_partition(G_igraph, la.ModularityVertexPartition,weights=weight)
    #modularity = partition.modularity
    modularity = G_igraph.modularity(partition.membership, weights= weight)


    print("The modularity is {}".format(modularity))
    
    # Create partition DataFrame
    partition_df = pd.DataFrame({'node': G_igraph.vs['name'], 'partition': partition.membership})
    print("The number of partitions is {}".format(len(partition_df['partition'].unique())))
    
    partition_df.rename(columns={'index':'node'}, inplace=True)
    textlist = ["The modularity is {}".format(modularity), "The number of partitions is {}".format(len(partition_df['partition'].unique()))]
    
    return partition_df, textlist

def leidenForFilesInFolder(rootdir, flowfilephrase, source, target, weight):
    
    for subdir, dirs, files in os.walk(rootdir):
        for file in files:
            if ((flowfilephrase in file) and (".csv" in file)):
                filename = file[-13:]
                df = pd.read_csv(os.path.join(subdir, file))
            
                partition, text = leidenPartitions(df, source, target, weight)

                newpath = subdir+"/Leiden_partition"

                if not os.path.exists(newpath):
                    os.makedirs(newpath)

                partition.to_csv(newpath+"/Leiden_partition_"+filename, index=False)
            
                with open(newpath+"/Leiden_partition_summary_"+filename[:-4]+".txt",'w') as f:
                    f.write('\n'.join(text))
                    f.close()
