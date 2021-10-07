%cMapFunc = @(x)1-gray(x);
isiAvgThresh = 20;
isiBurstThresh = 80;
cMapFunc = @jet;
[~,T_s,xR_s,isi_s] = plotISISurf(isiAvg, isiDataLabels{xCol}, isiDataLabels{yCol}, cMapFunc, isiAvgThresh, isiBurstThresh, [0, 0.6, -0.6, 0]);
view(gca,2)