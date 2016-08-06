#include "mainwindow.h"
#include <iostream>

int main(int argc, char *argv[])
{
	if(argc != 3)
	{
		std::cout << "USAGE: ./visualizer problem.txt image.png" <<std::endl;
		exit(1);
	}

	Drawer w;
	w.DrawProblem(argv[1], argv[2]);
//	w.DrawProblem("/Users/fedor/PROJECTS/Fun/icfpc-2016/problems/90.txt", "1.png");
	exit(0);
}
