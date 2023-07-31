import os
import gzip
import re 

# Reads the human reference genome and divide it into one file per chromosome 

input_file = "hg38.fa.gz"
output_dir = "chromosome_files"

if not os.path.exists(output_dir):
    os.makedirs(output_dir)

with gzip.open(input_file, "rt") as f:

    current_chromosome = None
    current_sequence = ""

    for line in f:
        #  ">" indicate the start of a new chromosome
        pattern = "^>chr[A-Za-z0-9]{1,2}$"
        if re.match(pattern, line):

            if current_chromosome is not None:
                output_file = os.path.join(output_dir, f"{current_chromosome}.fa")
                with open(output_file, "w") as out:
                    out.write(current_chromosome_line + "\n")  
                    out.write(current_sequence)

            current_chromosome = line.strip()[1:]
            current_sequence = ""
            current_chromosome_line = line.strip() 

        # no ">" : sequence data for the current chromosome
        else:
            current_sequence += line.strip()

    output_file = os.path.join(output_dir, f"{current_chromosome}.fa")
    with open(output_file, "w") as out:
        out.write(current_chromosome_line + "\n")  
        out.write(current_sequence)
