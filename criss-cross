:- dynamic word/1.
:- dynamic words/1.
:- dynamic matrix/1.
:- dynamic writedWords/1.
:- dynamic map/1.

:- dynamic score/1.

space(32).

clear() :- retractall(word(_)), retractall(words(_)), retractall(matrix(_)), 
		   retractall(map(_)), retractall(writedWords(_)).

loading(File) :- read_words(File), creat_words_list.%, create_matrix, create_map, create_writedWords.

/* Create Words List */ 

	read_words(Data_file) :- clear, see(Data_file),
			                repeat, read(F),
				                (F = end_of_file, !, seen;
				                atom_codes(F, Codes), assert(word(Codes)), fail).

	creat_words_list() :- findall(W, word(W), AllWords), sort_words(AllWords, SortedWords_Len), get_only_words(SortedWords_Len, SortedWords), 
						  assert(words(SortedWords)), retractall(word(_)).

	sort_words(AllWords, SortedWords) :- get_lens(AllWords, Word_len), merge_sort(Word_len, SortedWords).

	get_lens([], []).
	get_lens([W | TW], [W/L | T]) :- length(W, L), get_lens(TW, T).

	get_only_words([], []).
	get_only_words([W/_ | T], [W | Res]) :- get_only_words(T, Res).

		/* Merge sort */
		merge_sort([], []) :- !.
		merge_sort([X | []], [X]) :- !.
		merge_sort(L1, L2) :- split(L1, L11, L12), merge_sort(L11, L11_sort), merge_sort(L12, L12_sort), merge(L11_sort, L12_sort, L2).

		split([], [], []).
		split([X, Y | T], [X | L1], [Y | L2]) :- split(T, L1, L2), !.
		split(X, X, []).

		max(WX/X, _/Y, WX/X) :- X > Y, !.
		max(_/X, WY/Y, WY/Y) :- X < Y, !.
		max(WX/X, WY/Y, Winner) :- sort_litera(WX/X, WY/Y, Winner).

		sort_litera([L1 | T1]/N1, [L2 | T2]/N2, Winner) :- (L1 = L2, sort_litera(T1/N1, T2/N2, Winner), !;
															L1 > L2, Winner = [L1 | T1]/N1, !;
															Winner = [L2 | T2]/N2
															).

		merge([], L2, L2) :- !.
		merge(L1, [], L1) :- !.
		merge([W1/X1 | T1], [W2/X2 | T2], [WM/Max | Res]) :- max(W1/X1, W2/X2, WM/Max), (dif(W1, WM), !, merge([W1/X1 | T1], T2, Res); 
																						 merge(T1, [W2/X2 | T2], Res)
																						).

/* Get word by number */

	get_wordN(N, Word) :- words(Words), getN(N, Words, Word).

	getN(1, [X | _], X) :- !.
	getN(N, [_ | T], X) :- N1 is N - 1, getN(N1, T, X).


/* Create Matrix */

	create_matrix(Matrix) :- get_total_len(Size_tmp), Size is Size_tmp + 1, space_list(Size, List), list_of_lists(Size, List, Matrix).

	get_total_len(Sum_len) :- words(Words), sum_len(Words, Sum_len).

	sum_len([], 0).
	sum_len([W | T], Sum) :- sum_len(T, TS), length(W, L), Sum is TS + L.

	space_list(0, []) :- !.
	space_list(N, [32 | L1]) :- N1 is N - 1, space_list(N1, L1).

	list_of_lists(0, _, []) :- !.
	list_of_lists(N, List, [List | TL]) :- N1 is N - 1, list_of_lists(N1, List, TL).

/* Matrix operations */ 

	get_elem_matrix(Elem, X/Y, Matrix) :- getN(X, Matrix, Line), getN(Y, Line, Elem).



	set_elem_matrix(Elem, X/Y, Matrix, NewMatrix) :- getN(X, Matrix, Line), set_elem_list(Elem, Y, Line, NewLine), set_elem_list(NewLine, X, Matrix, NewMatrix).

	set_elem_list(E, 1, [_ | T], [E | T]) :- !.
	set_elem_list(E, Pos, [X | T], [X | Res]) :- Pos1 is Pos - 1, set_elem_list(E, Pos1, T, Res).


	
	/* Chech word for write */ 
		check_word([], _, _, _).
		check_word([Letter | T], X/Y, Dest, Matrix) :- get_elem_matrix(Elem, X/Y, Matrix), XU is X - 1, XD is X + 1, YL is Y - 1, YR is Y + 1, (Letter = Elem, !; space(S), Elem = S,
																							(Dest = 0, !, check_empty(XU/Y, Matrix), check_empty(XD/Y, Matrix), check_empty(X/YL, Matrix);
																							 check_empty(XU/Y, Matrix), check_empty(X/YL, Matrix), check_empty(X/YR, Matrix)
																							)
																			), (Dest = 0, !, check_tail_word(T, X/YR, Dest, Matrix);
																							 check_tail_word(T, XD/Y, Dest, Matrix)
																				).

		check_tail_word([], _, _, _).
		check_tail_word([Letter | []], X/Y, Dest, Matrix) :- get_elem_matrix(Elem, X/Y, Matrix), (Letter = Elem, !; space(S), Elem = S, XU is X - 1, XD is X + 1, YL is Y - 1, YR is Y + 1,
																							(Dest = 0, !, check_empty(XU/Y, Matrix), check_empty(XD/Y, Matrix), check_empty(X/YR, Matrix);
																							 check_empty(XD/Y, Matrix), check_empty(X/YL, Matrix), check_empty(X/YR, Matrix)
																							)
																				  ), !.
		check_tail_word([Letter | T], X/Y, Dest, Matrix) :- get_elem_matrix(Elem, X/Y, Matrix), XU is X - 1, XD is X + 1, YL is Y - 1, YR is Y + 1, (Letter = Elem, !; space(S), Elem = S, 
																							(Dest = 0, !, check_empty(XU/Y, Matrix), check_empty(XD/Y, Matrix);
																							 check_empty(X/YL, Matrix), check_empty(X/YR, Matrix)
																							)
																				  ), (Dest = 0, !, check_tail_word(T, X/YR, Dest, Matrix);
																					  check_tail_word(T, XD/Y, Dest, Matrix)
																					 ).		

		check_empty(X/Y, Matrix) :- get_elem_matrix(Elem, X/Y, Matrix), space(S), Elem = S.																																



	add_word([], _, _, Matrix, Matrix).
	add_word([Letter | T], X/Y, Dest, Matrix, NewMatrix2) :- set_elem_matrix(Letter, X/Y, Matrix, NewMatrix), (Dest = 0, X1 is X, Y1 is Y + 1, !; 
																		X1 is X + 1, Y1 is Y
																	   ), add_word(T, X1/Y1, Dest, NewMatrix, NewMatrix2).

	/* Print Matrix */
		print_matrix_memory() :- matrix(Matrix), print_matrix(Matrix).

		print_matrix([]).
		print_matrix([Line | T]) :- print_line(Line), print_matrix(T).

		print_line([]) :- nl.
		print_line([Letter | T]) :- put(Letter), print_line(T).


/* Create map overlapping */

	create_map(Map) :- words(Words), get_all_pairs(Words/1, Words/1, Map).

	get_all_pairs([]/_, _, []) :- !.
	get_all_pairs(_, []/_, []) :- !.
	get_all_pairs([W1 | T1]/N1, L2/1, Pairs) :- get_pairs(W1/N1, L2/1, Pairs1), N1_new is N1 + 1, get_all_pairs(T1/N1_new, L2/1, Pairs2), 
												 append(Pairs1, Pairs2, Pairs).

	get_pairs(_, []/_, []) :- !.
	get_pairs(W1/N, [_ | T2]/N, Pairs) :- !, N2_new is N + 1, get_pairs(W1/N, T2/N2_new, Pairs).
	get_pairs(W1/N1, [W2 | T2]/N2, Pairs) :- find_overlaps(W1/1, W2/1, Pairs1), append_pref(N1/N2, Pairs1, Pairs11), 
											 N2_new is N2 + 1, get_pairs(W1/N1, T2/N2_new, Pairs2), 
											 append(Pairs11, Pairs2, Pairs).

	find_overlaps([]/_, _, []) :- !.
	find_overlaps([Letter1 | T1]/N1, W2/1, Pairs) :- find_letter_same(Letter1/N1, W2/1, Pairs1), N1_new is N1 + 1, find_overlaps(T1/N1_new, W2/1, Pairs2), 
													  append(Pairs1, Pairs2, Pairs).

	find_letter_same(_, []/_, []) :- !.
	find_letter_same(Letter1/N1, [Letter1 | T2]/N2, [N1/N2 | Pairs]) :- !, N2_new is N2 + 1, find_letter_same(Letter1/N1, T2/N2_new, Pairs).
	find_letter_same(Letter1/N1, [_ | T2]/N2, Pairs) :- N2_new is N2 + 1, find_letter_same(Letter1/N1, T2/N2_new, Pairs).

	append_pref(_, [], []) :- !.
	append_pref(Pref, [W | T], [Pref/W | Res]) :- append_pref(Pref, T, Res).


/* Operations list of writed words*/
	create_writedWords(Matrix, NewMatrix, NewWritedWords, Map, NewMap) :- 
							get_wordN(1, Word), length(Matrix, Size), length(Word, Len),
							S is div(Size, 2), L is div(Len, 2), X is S - L, Y is X, WritedWords = [],
							full_write_word(1, X/Y, 0, Matrix, NewMatrix, WritedWords, NewWritedWords, Map, NewMap).
							

	add_writed_word(N, X/Y, Dest, WritedWord, [N/X/Y/Dest | WritedWord]).


/* Full add word */

	full_write_word(N, Pos, Dest, Matrix, NewMatrix, WritedWords, NewWritedWords, Map, NewMap) :- get_wordN(N, Word), 
									 add_word(Word, Pos, Dest, Matrix, NewMatrix), 
									 add_writed_word(N, Pos, Dest, WritedWords, NewWritedWords), 
									 delete_N_From_Map(N, Map, NewMap_tmp),
									 filter_map(NewMap_tmp, NewMap, NewMatrix, NewWritedWords).

	delete_N_From_Map(_, [], []) :- !. 								 
	delete_N_From_Map(N2, [_/N2/(_/_) | T], Res) :- !, delete_N_From_Map(N2, T, Res).
	delete_N_From_Map(N2, [Other_item | T], [Other_item | Res]) :- delete_N_From_Map(N2, T, Res).

	/* Filter map */								 
		filter_map([], [],  _, _) :- !.
		filter_map([V | T], [V | Res], Matrix, WritedWords) :- check_word2_overlap(V, Matrix, WritedWords), !, filter_map(T, Res, Matrix, WritedWords).
		filter_map([_ | T], Res, Matrix, WritedWords) :- filter_map(T, Res, Matrix, WritedWords).

		check_word2_overlap(N1/N2/(Pos1/Pos2), Matrix, WritedWords) :- getN_in_writedWords(N1, WritedWords, X/Y, Dest), (X = -1, !; 
																	   get_pos_in_matrix(Pos1/Pos2, X/Y, Dest, NewX, NewY),
																	   invert(Dest, NewDest), get_wordN(N2, WordN2),
																	   check_word(WordN2, NewX/NewY, NewDest, Matrix)
																	   ).

		getN_in_writedWords(_, [], X/X, X) :- !, X = -1.
		getN_in_writedWords(N, [N/X/Y/Dest | _], X/Y, Dest) :- !.
		getN_in_writedWords(N, [_ | T], X/Y, Dest) :- getN_in_writedWords(N, T, X/Y, Dest).

		get_pos_in_matrix(Pos1/Pos2, X/Y, Dest, NewX, NewY) :- (Dest = 0, !, NewX is X - (Pos2 - 1), NewY is Y + (Pos1 - 1);
																		     NewX is X + (Pos1 - 1), NewY is Y - (Pos2 - 1)
															   ).

		invert(0, 1).
		invert(1, 0).


run_cross() :- create_matrix(Matrix), create_map(Map), create_writedWords(Matrix, NewMatrix, WritedWords, Map, NewMap),
			   add_next_word(NewMatrix, NewMap, WritedWords).

/* Add next word */
	add_next_word(Matrix, Map, WritedWords) :- get_next_word(Map, WritedWords, N, Pos, Dest), 
				   full_write_word(N, Pos, Dest, Matrix, NewMatrix, WritedWords, NewWritedWords, Map, NewMap), 
				   words(Words), length(Words, Cnt_words), length(WritedWords, Len_curr),
				   (Cnt_words = Len_curr, !, save_cross(NewMatrix);
				   	print_matrix(NewMatrix), add_next_word(NewMap, NewMatrix, NewWritedWords)
				   ).

	get_next_word(Map, WritedWords, N, Pos, Dest) :- get_first_word(Map, WritedWords, N, Pos, Dest).

	get_first_word(Map, [WritedWord | _], N_next, Pos_next, Dest_next) :- get_first_word_for_writed(Map, WritedWord, N_next, Pos_next, Dest_next).
	get_first_word(Map, [_ | T_WW], N_next, Pos_next, Dest_next) :- get_first_word(Map, T_WW, N_next, Pos_next, Dest_next).

	get_first_word_for_writed([N/N2/(Pos1/Pos2) | _], N/X/Y/Dest, N2, NewX/NewY, NewDest) :- 
										get_pos_in_matrix(Pos1/Pos2, X/Y, Dest, NewX, NewY), invert(Dest, NewDest).
	get_first_word_for_writed([_ | T], WritedWord, N_next, Pos_next, Dest_next) :- get_first_word_for_writed(T, WritedWord, N_next, Pos_next, Dest_next).







save_cross(Matrix) :- assert(matrix(Matrix)).










%test(Data_file, C) :- see(Data_file), get(C), put(C), seen.
%TX is NewX + 97, TY is NewY + 97, put(TX), put(TY), put(32),



























