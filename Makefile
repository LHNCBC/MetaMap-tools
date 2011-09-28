#!/usr/bin/make -f 
# Top-level makefile for tools directory.
#
# To use set environment variable "SKR" to location of SKR directory,
# usually $(HOME)/specialist/SKR.
#
#   $ make SKR=$(HOME)/specialist/SKR
#
# In this case the sub-directory Makefiles includes Makefile.include
# (Makefile is used implicitly).
#
#
all : compile_build_ambig_examples compile_extract_mrconso_sources compile_filter_mrconso \
compile_glean_mrcon compile_filter_mrconso compile_flip_variants compile_glean_ambig \
compile_glean_mrcon compile_mm_print compile_mm_tokenizer compile_mm_variants compile_prefilter_mrconso


compile_build_ambig_examples :
	cd build_ambig_examples && $(MAKE) all

compile_extract_mrconso_sources :
	cd extract_mrconso_sources && $(MAKE) all

compile_filter_mrconso :
	cd filter_mrconso && $(MAKE) all

compile_flip_variants :
	cd flip_variants && $(MAKE) all

compile_glean_ambig :
	cd glean_ambig && $(MAKE) all

compile_glean_mrcon :
	cd glean_mrcon && $(MAKE) all

compile_mm_print :
	cd mm_print && $(MAKE) all

compile_mm_tokenizer :
	cd mm_tokenizer && $(MAKE) all

compile_mm_variants :
	cd mm_variants && $(MAKE) all

compile_prefilter_mrconso :
	cd prefilter_mrconso && $(MAKE) all

clean : clean_build_ambig_examples clean_extract_mrconso_sources clean_filter_mrconso \
clean_glean_mrcon clean_filter_mrconso clean_flip_variants clean_glean_ambig \
clean_glean_mrcon clean_mm_print clean_mm_tokenizer clean_mm_variants clean_prefilter_mrconso

clean_build_ambig_examples :
	cd build_ambig_examples && $(MAKE) clean

clean_extract_mrconso_sources :
	cd extract_mrconso_sources && $(MAKE) clean

clean_filter_mrconso :
	cd filter_mrconso && $(MAKE) clean

clean_flip_variants :
	cd flip_variants && $(MAKE) clean

clean_glean_ambig :
	cd glean_ambig && $(MAKE) clean

clean_glean_mrcon :
	cd glean_mrcon && $(MAKE) clean

clean_mm_print :
	cd mm_print && $(MAKE) clean

clean_mm_tokenizer :
	cd mm_tokenizer && $(MAKE) clean

clean_mm_variants :
	cd mm_variants && $(MAKE) clean

clean_prefilter_mrconso :
	cd prefilter_mrconso && $(MAKE) clean 

