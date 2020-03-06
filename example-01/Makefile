
all: Dmel_BDGP6.28_seqlens.tsv

Drosophila_melanogaster.BDGP6.28.dna.toplevel.fa.gz: 
	wget ftp://ftp.ensembl.org/pub/release-99/fasta/drosophila_melanogaster/dna/Drosophila_melanogaster.BDGP6.28.dna.toplevel.fa.gz

Dmel_BDGP6.28_seqlens.tsv: Drosophila_melanogaster.BDGP6.28.dna.toplevel.fa.gz
	bioawk -c fastx '{print $$name "\t" length($$seq)}' Drosophila_melanogaster.BDGP6.28.dna.toplevel.fa.gz > Dmel_BDGP6.28_seqlens.tsv


