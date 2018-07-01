// sectionsort.cpp : 定义控制台应用程序的入口点。
//

//#include "stdafx.h"
#include "math.h"
#include "stdio.h"
#include "stdlib.h"

int JudgeIntersection(double line[][2]) {
  double x1, x2, y1, y2, a, b, c, d, e, x, y;

  x1 = line[0][0];
  y1 = line[0][1];
  x2 = line[1][0];
  y2 = line[1][1];

  a = y2 - y1;
  b = (x1 - x2) / a;
  c = (x2 * y1 - x1 * y2) / a;

  x1 = line[2][0];
  y1 = line[2][1];
  x2 = line[3][0];
  y2 = line[3][1];

  a = y2 - y1;
  d = (x1 - x2) / a;
  e = (x2 * y1 - x1 * y2) / a;

  y = (e - c) / (b - d);
  x = -c - b * y;

  if ((x >= line[0][0] && x <= line[1][0]) ||
      (x >= line[2][0] && x <= line[3][0]))
    return 1; //线段相交
  else
    return 0;
  /*  已知直线上的两点P1(X1,Y1) P2(X2,Y2)， P1 P2两点不重合，直线方程为
      AX + BY + C = 0：
          A = Y2 - Y1
          B = X1 - X2
          C = X2*Y1 - X1*Y2

  */
}

void SortSection(double onesec[][2], int num) // 这里是否需要X Z方向坐标系标志？
{
  int i, j, k, pp1, pp2, istart, n;
  double temp, (*part1)[2], (*part2)[2], (*part3)[2], chord;
  double len1, len2, a, b, c, d, costhelta, threshold, thelta, xmax, xtol;
  double line[4][2], e, f, g, h, len3, len4;
  part1 = (double(*)[2])malloc(num * 2 * sizeof(double));
  part2 = (double(*)[2])malloc(num * 2 * sizeof(double));
  part3 = (double(*)[2])malloc(num * 2 * sizeof(double));
  FILE *fp = fopen("sorted.dat", "w");

  threshold = cos(40 / 57.296);
  for (i = 0; i < num; i++)
    for (j = i + 1; j < num; j++)
      if (onesec[i][0] > onesec[j][0])
        for (k = 0; k < 2; k++) {
          temp = onesec[i][k];
          onesec[i][k] = onesec[j][k];
          onesec[j][k] = temp;
        }
  chord = onesec[num - 1][0] - onesec[0][0];
  // 最后几个点按照Y坐标排序
  //	/*
  xtol = 1.0e-4;
  xmax = onesec[num - 1][0];
  for (i = num - 1; i > num - 30; i--)
    if (xmax - onesec[i][0] > xtol * chord)
      break;
  for (i++; i < num; i++)
    for (j = i + 1; j < num; j++)
      if (onesec[i][1] < onesec[j][1])
        for (k = 0; k < 2; k++) {
          temp = onesec[i][k];
          onesec[i][k] = onesec[j][k];
          onesec[j][k] = temp;
        }
  //	*/
  for (i = 1; i < 4; i++) // 头4个点按照Y排序，避免X坐标相等的情况出问题
    if (fabs(onesec[i][0] - onesec[0][0]) > 1.0e-8)
      break;
  n = i;
  for (i = 0; i < n; i++)
    for (j = i + 1; j < n; j++)
      if (onesec[i][1] > onesec[j][1])
        for (k = 0; k < 2; k++) {
          temp = onesec[i][k];
          onesec[i][k] = onesec[j][k];
          onesec[j][k] = temp;
        }

  part1[0][0] = onesec[0][0]; // part1上表面  part2 下表面
  part1[0][1] = onesec[0][1];
  part2[0][0] = onesec[0][0]; // part1上表面  part2 下表面
  part2[0][1] = onesec[0][1];
  pp1 = 2;
  pp2 = 1;
  for (i = 1; i < num; i++)
    if (onesec[i][1] > onesec[0][1])
      break;
    else {
      part2[pp2][0] = onesec[i][0];
      part2[pp2][1] = onesec[i][1];
      pp2++;
    }
  part1[1][0] = onesec[i][0];
  part1[1][1] = onesec[i][1];
  istart = i;
  i++;

  for (; i < num; i++) {
    if (i >= 337) {
      printf(" i=%5d\n", i);
    }
    if (part1[pp1 - 1][0] - part1[0][0] < chord * 0.1)
      threshold = cos(50 / 57.296);
    else
      threshold = cos(10 / 57.296);
    a = part1[pp1 - 1][0] - part1[pp1 - 2][0];
    b = part1[pp1 - 1][1] - part1[pp1 - 2][1];
    len1 = sqrt(a * a + b * b);
    istart++;
  a1:
    for (j = istart; j < num; j++) {
      c = onesec[j][0] - part1[pp1 - 1][0];
      d = onesec[j][1] - part1[pp1 - 1][1];
      len2 = sqrt(c * c + d * d);
      if (fabs(len2) < 1.0e-10)
        break; // 如果有重复点，则是后缘点，不用再排了
      costhelta = (a * c + b * d) / len1 / len2;
      if (costhelta > 1.0)
        costhelta = 0.9999997;
      thelta = acos(costhelta) * 57.293;
      if (i > num - 10)
        printf(" thelta= %lf\n", thelta);

      if (costhelta > threshold) {
        // 相交判断
        if (j > num - 30 && j < num - 1) //
        {
          e = part2[pp2 - 1][0] - part2[pp2 - 2][0];
          f = part2[pp2 - 1][1] - part2[pp2 - 2][1];
          len3 = sqrt(e * e + f * f);
          g = onesec[j][0] - part2[pp2 - 1][0];
          h = onesec[j][1] - part2[pp2 - 1][1];
          len4 = sqrt(g * g + h * h);
          costhelta = (e * g + f * h) / len3 / len4;
          if (costhelta > threshold) // 这个点与上下表面的夹角都<10度才需要判断
          {
            line[0][0] = part1[pp1 - 1][0];
            line[0][1] = part1[pp1 - 1][1];
            line[1][0] = onesec[j][0];
            line[1][1] = onesec[j][1];
            line[2][0] = part2[pp2 - 1][0];
            line[2][1] = part2[pp2 - 1][1];
            line[3][0] = onesec[j + 1][0];
            line[3][1] = onesec[j + 1][1];
            if (JudgeIntersection(line) == 1) {
              part2[pp2][0] = onesec[j][0];
              part2[pp2][1] = onesec[j][1];
              pp2++;
              continue;
            }
          }
        }
        // 有一种情况会判断错误
        /*
        ****************************
                                         *
        这个点会判断失误。确实相交，但确实是上表面
        ****************************                 *
        但是这种情况不会导致最后的上下表面相交，因此计算面积没问题，就是有一点小误差而已
        */
        part1[pp1][0] = onesec[j][0];
        part1[pp1][1] = onesec[j][1];
        pp1++;
        i = j - 1;
        istart = j;
        break;
      } else {
        part2[pp2][0] = onesec[j][0];
        part2[pp2][1] = onesec[j][1];
        pp2++;
      }
    }
    if (j == num && costhelta < threshold &&
        onesec[num - 1][1] > part1[pp1 - 1][1]) // 证明前一步搞错了，要回退
                                                /*  情况1
                                                ****************      *
                                                ---------------------*
                                                ******************
                                                */
    {
      pp1--;
      pp2--;
      a = part1[pp1 - 1][0] - part1[pp1 - 2][0];
      b = part1[pp1 - 1][1] - part1[pp1 - 2][1];
      len1 = sqrt(a * a + b * b);
      printf(" wrong\n\n");
      goto a1;
    } else if (j == num)
      break; // 达到了下图的上表面的后缘点
    /*  情况2
    ********************
    ---------------------*
    ----------------------*
    -----------------------*
    *************************
    */
  } // 上表面排序完成     台阶中间的点是需要舍弃的
  /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
  /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
  /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
  /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
  // 下表面排序是一样
  for (i = 0; i < pp1; i++)
    fprintf(fp, "%15.8lf  %15.8lf\n", part1[i][0], part1[i][1]);

  part2[pp2][0] = onesec[num - 1][0];
  part2[pp2][1] = onesec[num - 1][1];
  pp2++;
  for (i = 0; i < pp2; i++)
    for (j = i + 1; j < pp2; j++)
      if (part2[i][0] > part2[j][0])
        for (k = 0; k < 2; k++) {
          temp = part2[i][k];
          part2[i][k] = part2[j][k];
          part2[j][k] = temp;
        }

  // 最后几个点按照Y坐标排序
  //	/*
  xtol = 1.0e-4;
  xmax = part2[pp2 - 1][0];
  for (i = pp2 - 1; i > pp2 - 30; i--)
    if (xmax - part2[i][0] > xtol * chord)
      break;
  for (i++; i < pp2; i++)
    for (j = i + 1; j < pp2; j++)
      if (part2[i][1] > part2[j][1])
        for (k = 0; k < 2; k++) {
          temp = part2[i][k];
          part2[i][k] = part2[j][k];
          part2[j][k] = temp;
        }
  //	*/

  part3[0][0] = part2[0][0];
  part3[0][1] = part2[0][1];
  part3[1][0] = part2[1][0];
  part3[1][1] = part2[1][1];

  pp1 = 2;
  istart = 1;

  printf(" \n\n");

  for (i = 0; i < pp2; i++) {
    if (part3[pp1 - 1][0] - part3[0][0] < chord * 0.1)
      threshold = cos(50 / 57.296);
    else
      threshold = cos(10 / 57.296);

    a = part3[pp1 - 1][0] - part3[pp1 - 2][0];
    b = part3[pp1 - 1][1] - part3[pp1 - 2][1];
    len1 = sqrt(a * a + b * b);
    istart++;
  a2:
    for (j = istart; j < pp2; j++) {
      c = part2[j][0] - part3[pp1 - 1][0];
      d = part2[j][1] - part3[pp1 - 1][1];
      len2 = sqrt(c * c + d * d);
      if (fabs(len2) < 1.0e-10) {
        break; // 如果有重复点，则是后缘点，不用再排了
      }
      costhelta = (a * c + b * d) / len1 / len2;
      if (costhelta > 1.0)
        costhelta = 0.9999997;
      thelta = acos(costhelta) * 57.293;
      if (i > pp2 - 5) {
        printf(" thelta= %lf\n", thelta);
      }

      if (costhelta > threshold) {
        part3[pp1][0] = part2[j][0];
        part3[pp1][1] = part2[j][1];
        pp1++;
        i = j - 1;
        istart = j;
        break;
      }
    }
    if (j == pp2 && costhelta < threshold &&
        part2[pp2 - 1][1] < part3[pp1 - 1][1]) // 证明前一步搞错了，要回退
                                               /*  情况1
                                               ********************
                                               ---------------------*
                                               ----------------------*
                                               -----------------------*
                                               *******************     *
                                               */
    {
      pp1--;
      a = part3[pp1 - 1][0] - part3[pp1 - 2][0];
      b = part3[pp1 - 1][1] - part3[pp1 - 2][1];
      len1 = sqrt(a * a + b * b);
      goto a2;
    } else if (j == num)
      break; // 达到了下图的上表面的后缘点
    /*  情况2  以上表面为例
     */
  }
  fprintf(fp, "\n\n\n\n");
  for (i = pp1 - 1; i > -1; --i)
    fprintf(fp, "%15.8lf  %15.8lf\n", part3[i][0], part3[i][1]);
  fclose(fp);
  free(part1);
  free(part2);
  free(part3);
}

int main(int argc, char *argv[]) {
  FILE *fp;
  int i = 0, num = 0;
  double(*onesec)[2], temp;
  if (argc < 2)
    return 0;
  fp = fopen(argv[1], "r"); // 不允许有重复点，除了后缘点以外
  while (1) {
    fscanf(fp, "%lf %lf", &temp, &temp);
    if (!feof(fp))
      num++;
    else
      break;
  }
  rewind(fp);
  onesec = (double(*)[2])malloc(num * 2 * sizeof(double));
  for (i = 0; i < num; i++)
    fscanf(fp, "%lf %lf", &onesec[i][0], &onesec[i][1]);
  fclose(fp);

  SortSection(onesec, num); // 这里是否需要X Z方向坐标系标志？
  free(onesec);
  return 0;
}
