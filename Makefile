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
	cd build_ambig_examples && $(MAKE)

compile_extract_mrconso_sources :
	cd extract_mrconso_sources && $(MAKE)

compile_filter_mrconso :
	cd filter_mrconso && $(MAKE)

compile_flip_variants :
	cd flip_variants && $(MAKE)

compile_glean_ambig :
	cd glean_ambig && $(MAKE)

compile_glean_mrcon :
	cd glean_mrcon && $(MAKE)

compile_mm_print :
	cd mm_print && $(MAKE)

compile_mm_tokenizer :
	cd mm_tokenizer && $(MAKE)

compile_mm_variants :
	cd mm_variants && $(MAKE)

compile_prefilter_mrconso :
	cd prefilter_mrconso && $(MAKE) 

