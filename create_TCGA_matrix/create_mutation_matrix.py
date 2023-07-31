import pyreadr
from Bio import SeqIO
import time 
import pandas as pd
import sys 


def prepare_rds_file(mut_file_path):
    '''
    Takes the path/filename to a rds file, and reads the rds file. 
    The RDS file includes mutations in individuals, one row corresonds to one mutation in one individual
    Only keeps rows and columns of interest
    Returns a dataframe of mutations 


    Input: mut_file_path[str]
    Output: df[pandas.dataframe]

    '''
    # Read the file and time it 
    start_time_read = time.time()
    result = pyreadr.read_r(mut_file_path) 
    end_time_read = time.time()

    print("Time reading file:", end_time_read-start_time_read)

    # Make a dataframe with relevant columns and values 
    df = result[None]
    df = df.loc[(df['filter'] == 'PASS') & (df['start'] == df['end']) & (df['ref'].str.match('^[ATCG]$')) & (df['alt'].str.match('^[ATCG]$')), ['Sample_ID', 'chrom', 'start','ref', 'alt']]
    return df

def find_context(df):
    '''
    Seach throght the dataframe and finds the context in a reference genome. 
    Adds a column with the context and one that tells if the ref base corresponds in the 
    file and in the referance genome 
    
    Input: df[pandas.dataframe]
    Output: df[pandas.dataframe]
    '''

    contexts = []
    correct_ref = []
    start_t = time.time()
    for i in range(len(df.index)):

        chrx = str(df.iat[i,1])
        path_to_file = "chromosome_files/" + chrx + ".fa"
        record  = SeqIO.read(path_to_file, "fasta")

        pos1 = int(df.iat[i,2]) - 2
        pos2 = int(df.iat[i,2]) + 1
        cont = (record[pos1:pos2].seq).upper()
        contexts.append(''.join(cont))
        correct_ref.append(df.iat[i,3] == cont[1])

    end_t = time.time()

    print("Time executing the loop with", len(df.index), "elements:", end_t - start_t)

    df = df.assign(context = contexts)
    df = df.assign(correct_ref = correct_ref)


    sbs_false = df.loc[(df['correct_ref'] == False)]
    print("Number of worng referances:", len(sbs_false.index))
    if (len(sbs_false.index > 0)):
        print(sbs_false)

    df = df.loc[(df['correct_ref'] == True)]
    
    return df 

def create_empty_mutation_frame(file_row_names, ids_col):
    '''
    Creates a mutation category dataframe with correct row- and columnames. 
    Every entry is set to 0.

    Input: 
        file_row_names[str]
        ids_col[list]
    
    Output: df[pandas.dataframe]

    '''
    # Make the mutation matrix 
    with open(file_row_names) as f:
        cats = [cat.strip() for cat in f]

    
    df = pd.DataFrame(0, columns=cats, index=set(ids_col))

    return df

def write_to_file(df, filename):
    '''
    Writes a dataframe to file. Both .csv and .txt
    For the mutation datafile.
    The filename should be passed without file ending

    Input: 
        df[pandas.dataframe]
        filename[str]
    
    Output: None 
    '''
    df.to_csv(filename + '.csv')

    with open(filename + '.txt', 'w') as f:
        dfAsString = df.to_string(header=True, index=True, na_rep=0)
        f.write(dfAsString)

def fill_mutation_matrix(sbs_df, mut_df):
    '''
    Takes a mutation dataframe and a file with mutations and the context
    Loop through all mutations and add to the mutation matrix
    Retuns mutation matrix 

    Input: 
        sbs_df[pandas.dataframe]
        mut_df[pandas.dataframe]

    Output: mut_df[pandas_dataframe]
    '''

    comp_dict = {'A':'T', 'T':'A', 'C':'G', 'G':'C'}
    cats = mut_df.columns
    ids = mut_df.index

    for i in range(len(sbs_df.index)):
        context = sbs_df.iat[i,5]

        before_base = context[0]
        after_base = context[2]

        ref_base = sbs_df.iat[i,3]
        alt_base = sbs_df.iat[i,4]

        category_col = before_base + "[" + ref_base + ">" + alt_base + "]" + after_base
        comp_col = before_base + "[" + comp_dict[ref_base] + ">" + comp_dict[alt_base] + "]" + after_base

        id_row = sbs_df.iat[i,0]



        if category_col in set(cats):
            mut_df.loc[id_row, category_col] += 1
        else:
            mut_df.loc[id_row, comp_col] += 1
    
    return mut_df


# File path to files 
mut_file_path = 'TCGA-BRCA.mutect2_snv.rds'
category_file = 'categories.txt'

# Read the RDS file and add the context from reference genome 
df = prepare_rds_file(mut_file_path) 



if (len(sys.argv)) > 1:
    nr_of_elements = int(sys.argv[1])
    df = df[:nr_of_elements] # subset rds file 

context_df = find_context(df) 

# The unique ids for the mutation file 
ids = set(df.iloc[:,0])

print(ids)

# Create the Mutation Matrix M using mutations from rds file 
empty_mut_frame = create_empty_mutation_frame(category_file, ids) 
mut_matrix = fill_mutation_matrix(context_df, empty_mut_frame) 

# Write the dataframes to files 
write_to_file(mut_matrix, 'mutation_matrix')
write_to_file(context_df, 'context_sbs')

# Print the dataframes 
print(context_df)
print(mut_matrix)
