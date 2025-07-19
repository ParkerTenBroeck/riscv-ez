

void meow();

int this_is_a_test(){
	return 3;
}

int(*funky)() = this_is_a_test;

int nyaaa(){
	return funky();
}

int start_(int test){
	if(test){
		meow();
	}
	return this_is_a_test();
}
