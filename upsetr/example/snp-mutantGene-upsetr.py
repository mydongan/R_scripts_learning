###################################################################################################
#!/usr/bin/python
# -*- coding:UTF-8 -*-
'''
根据snp.annotation.xls or indel.annotation.xls筛选筛选出每个样本突变的基因（有无突变，0或者1），不重复累加，即多次突变也计数为1
Example:
python snp-mutantGene-stat.py -i snp.annotation.xls -o sample.mutantGene.xls -s sample

作者：andong
2023-11-15
'''
import pdb
import os,sys
import re,io
import click
from collections import defaultdict


sampleName=''
filename=''

def extract_specifi_sample_Mutgene(infile,outfile,sample):
    genedic=defaultdict(dict)
    sampledic={}
    samplelist=[]
    mutantlist=[]
    with open (infile,'r') as files,open (outfile,'w') as outfile, open(sample,'r') as sample:
        for sampleid in sample:
            sampleid=sampleid.strip()
            samplelist.append(sampleid)
        outfile.write('Gene'+'\t'+('\t').join(samplelist)+'\n')


        header=files.readline() #read the first line
        #print(header)
        index=header.strip().split('\t').index('Gene.refGene')
        #print(index) 
        for j in range(5,index-1):
            sampleName=header.strip().split('\t')[j]
            sampledic[j]=sampleName

        for line in files:
            line=line.strip().split('\t')
            genes=re.split(r'[,|;]',line[index])
            genes = [gene for gene in genes if gene != ""]
            #print(genes)
            for j in range(5,index-1):
                sampleName=sampledic[j]
                #print(sampleName)
                genetype = line[j].split(' | ')[0]
                #print(genetype)
                if sampleName in samplelist:
                    if genetype =='0/0' or genetype=='./.':  #或判断  这块逻辑很重要，第一次出现设定为0，之后再出现也不替换前面的结果
                        for gene in genes:
                            if gene != "None": 
                                if gene not in genedic:
                                    genedic[gene] = {}
                                    if sampleName not in genedic[gene]:
                                        genedic[gene][sampleName] = 0
                                else:
                                    if sampleName not in genedic[gene]:
                                        genedic[gene][sampleName] = 0

                    elif  genetype =='0/1' or genetype=='1/1':
                        for gene in genes:
                            if gene != "None":
                                genedic[gene][sampleName]=1
            #print(genedic)

        for gene in genedic.keys():
            mutantlist=[]
            for sample in samplelist:
                #print(sample)
                if sample in genedic[gene].keys():
                    mutant=str(genedic[gene][sample])
                    #print(mutant)
                    mutantlist.append(mutant)            
            outfile.write(gene+'\t'+'\t'.join(mutantlist)+'\n')
    

@click.command()
@click.option('-i','--infile', help='请输入snp.annotation.xls文件')
@click.option('-o','--outfile', help='请输入输出sample.mutantGene.xls文件')
@click.option('-s','--sample', help='请输入样本名文件')
def main(infile,outfile,sample):
    extract_specifi_sample_Mutgene(infile,outfile,sample)
if __name__ == '__main__':
    main() 
