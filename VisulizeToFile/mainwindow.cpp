#include "mainwindow.h"
#include <QImage>
#include <QPainter>
#include <QFile>
#include <iostream>
#include <sstream>

double ReadRational(std::stringstream & ss)
{
	double a,b;
	char c;
	ss >> a >> c;
	if(c == '/')
	{
		ss >> b;
		return a /= b;
	}
	ss.putback(c);
	return a;
}

Drawer::Problem Drawer::ReadProblem(const char * Src)
{
	Problem problem;
	QFile file(Src);
	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		std::cout << "Invalid file name: " << Src << std::endl;
		exit(1);
	}

	QByteArray text = file.readAll();
	std::stringstream ss(text.toStdString().c_str());

	unsigned int poly_cnt;
	ss >> poly_cnt;
	for (unsigned int poly_n=0; poly_n < poly_cnt; ++poly_n)
	{
		QVector<QPointF> poly;
		unsigned int points_cnt;
		ss >> points_cnt;
		for (unsigned int point_n=0; point_n < points_cnt; ++point_n)
		{
			double x = ReadRational(ss);
			char c;
			ss >> c;
			if(c != ',')
			{
				qWarning("Invalid format. File '%s' poly #%i point #%i: %c", Src, poly_n+1, point_n+1, c);
				exit(1);
			}
			double y = ReadRational(ss);
			poly.push_back(QPointF(x, y));
		}
		problem.polygons.push_back(poly);
	}

	unsigned int edges_cnt;
	ss >> edges_cnt;
	for (unsigned int edge_n=0; edge_n < edges_cnt; ++edge_n)
	{
		double x1 = ReadRational(ss);
		char c;
		ss >> c;
		if(c != ',')
		{
			qWarning("Invalid format. File '%s' edge #%i: %c", Src, edge_n+1, c);
			exit(1);
		}
		double y1 = ReadRational(ss);

		double x2 = ReadRational(ss);
		ss >> c;
		if(c != ',')
		{
			qWarning("Invalid format. File '%s' edge #%i: %c", Src, edge_n+1, c);
			exit(1);
		}
		double y2 = ReadRational(ss);
		problem.skeleton.push_back(QLineF(x1, y1, x2, y2));
	}

	return problem;
}

Drawer::Solution Drawer::ReadSolution(const char * Src)
{
	Solution solution;
	QFile file(Src);
	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		std::cout << "Invalid file name: " << Src << std::endl;
		exit(1);
	}

	QByteArray text = file.readAll();
	std::stringstream ss(text.toStdString().c_str());

	unsigned int points_cnt;
	ss >> points_cnt;
	for (unsigned int point_n=0; point_n < points_cnt; ++point_n)
	{
		double x = ReadRational(ss);
		char c;
		ss >> c;
		if(c != ',')
		{
			qWarning("Invalid format. File '%s' src point #%i : %c", Src, point_n+1, c);
			exit(1);
		}
		double y = ReadRational(ss);
		solution.src.push_back(QPointF(x, y));
	}


	unsigned int poly_cnt;
	ss >> poly_cnt;
	for (unsigned int poly_n=0; poly_n < poly_cnt; ++poly_n)
	{
		Facet facet;
		unsigned int points_cnt;
		ss >> points_cnt;
		for (unsigned int point_n=0; point_n < points_cnt; ++point_n)
		{
			int n;
			ss >> n;
			facet.push_back(n);
		}
		solution.facets.push_back(facet);
	}

	for (unsigned int point_n=0; point_n < points_cnt; ++point_n)
	{
		double x = ReadRational(ss);
		char c;
		ss >> c;
		if(c != ',')
		{
			qWarning("Invalid format. File '%s' dst point #%i : %c", Src, point_n+1, c);
			exit(1);
		}
		double y = ReadRational(ss);
		solution.dst.push_back(QPointF(x, y));
	}

	return solution;
}



void Drawer::DrawProblem(const char * ProblemSrc, const char * SolutionSrc, const char * Dst)
{	

	int w = 400;
	int h = 400;
	QImage image(w + 300, h + 300, QImage::Format_RGB32);
	QPainter painter(&image);

	if(ProblemSrc)
	{
		Problem problem = ReadProblem(ProblemSrc);
		painter.setPen(QColor(Qt::white));

		double min_x = problem.polygons.front().front().rx();
		double min_y = problem.polygons.front().front().ry();
		double max_x = problem.polygons.front().front().rx();
		double max_y = problem.polygons.front().front().ry();
		for(int poly_n = 0; poly_n < problem.polygons.size(); ++poly_n)
		{
			for(unsigned int point_n = 0; point_n < problem.polygons[poly_n].size(); ++point_n)
			{
				min_x = std::min(min_x, problem.polygons[poly_n][point_n].rx());
				min_y = std::min(min_y, problem.polygons[poly_n][point_n].ry());
				max_x = std::max(min_x, problem.polygons[poly_n][point_n].rx());
				max_y = std::max(min_y, problem.polygons[poly_n][point_n].ry());
			}
		}

		painter.translate(50 - min_x*w,50 - min_y*h);

		for(int poly_n = 0; poly_n < problem.polygons.size(); ++poly_n)
		{
			unsigned int size = problem.polygons[poly_n].size();
			for(unsigned int point_n = 0; point_n < size; ++point_n)
			{
				painter.drawLine(problem.polygons[poly_n][point_n].rx()*w, problem.polygons[poly_n][point_n].ry()*h,
								problem.polygons[poly_n][(point_n+1)%size].rx()*w, problem.polygons[poly_n][(point_n+1)%size].ry()*h);
			}
		}

		for(int edge_n = 0; edge_n < problem.skeleton.size(); ++edge_n)
			painter.drawLine(problem.skeleton[edge_n].x1()*w, problem.skeleton[edge_n].y1()*h,
						problem.skeleton[edge_n].x2()*w, problem.skeleton[edge_n].y2()*h);
	}

	if(SolutionSrc)
	{
		Solution solution = ReadSolution(SolutionSrc);
		painter.setPen(QColor(Qt::red));

		for(int poly_n = 0; poly_n < solution.facets.size(); ++poly_n)
		{
			Facet facet = solution.facets[poly_n];
			for(unsigned int point_n = 0; point_n < facet.size(); ++point_n)
			{
				QPointF p1 = solution.dst[facet[point_n]];

				QPointF p2 = solution.dst[facet[(point_n+1)%facet.size()]];
				painter.drawLine(p1.rx() * w, p1.ry() * h, p2.rx() * w, p2.ry() * h);
			}
		}
	}



	image.save(Dst);
}
