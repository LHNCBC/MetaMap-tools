
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

% File:     factbase.pl
% Module:   Factbase
% Author:   Lan
% Purpose:  Provides for the storage, retrieval and management of facts
%           characterized by having a Key, Property and Value each of which
%           is an arbitrary term (atomic Keys are preferred for efficiency).


/* Factbase Module
*/

:- module(factbase,[
    put_fact/3,
    get_fact/3,
    erase_fact/3,
    erase_all_facts/2
    ]).


:- dynamic 'FactBase'/3.


/* put_fact(+Key, +Property, +Value)
*/

put_fact(Key,Property,Value) :-
    assertz('FactBase'(Key,Property,Value)).

/* get_fact(+Key, ?Property, ?Value)
   get_set_of_all_values(+Key, +Property, -ValueSet)
*/

get_fact(Key,Property,Value) :-
    'FactBase'(Key,Property,Value).

/* 
   erase_all_facts(+Key, +Property)
*/

erase_all_facts(Key,Property) :-
    retractall('FactBase'(Key,Property,_)),
    !.

/* erase_fact(+Key, +Property, ?Value)
*/

erase_fact(Key,Property,Value) :-
    retract(('FactBase'(Key,Property,Value) :- true)),
    !.

