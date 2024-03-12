sampleID = $1
bowtie2 -p 20 -x UHGP_index -1 $sampleID.rm.1.fq.gz -2 $sampleID.rm.2.fq.gz > $sampleID.sam
samtools view -bS -@ 20 $sampleID.sam > $sampleID.bam 
samtools sort $sampleID.bam $sampleID.sort 
sh mapping_rate.sh $sampleID
jgi_summarize_bam_contig_depths --outputDepth $sampleID.depth $sampleID.sort.bam
