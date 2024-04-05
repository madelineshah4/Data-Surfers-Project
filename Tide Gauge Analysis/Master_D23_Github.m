% This script performs the nonlinear trend analysis for all tide gauges
% along the U.S. coastline as described in the paper:
% Dangendorf, S., Hendricks, N., Sun, Q., Klinck, J., Ezer, T., Frederikse,
% T., Calafat, F.M., Wahl, T., Tornqvist, T.

%% (1) LOAD ALL PREPROCESSED MSL TIME SERIES FROM PSMSL AND THEIR INDIVIDUAL 
% CONTRIBUTIONS AS DETERMINED AND PREPROCESSED IN THE PAPER. THEY ARE ALL
% MONTHLY AVERAGES BUT ALREADY DESEASONALIZED

% L = [Lon,Lat]
% MID = PSMSL ID
% N = Station Name
% t = time
% Mds_upd = Original deseasoned MSL records
% Mfil_upd = Deseasoned MSL records, now with gaps filled
% Mfilc = Deseasoned MSL records, now with gaps filled, and VLM correction applied
% MBSL = Barotropic coastal wind and inverse barometer effects
% MIBE = Inverse barometer contribution
% MGRD = Barystatid GRD
% MVLM = Vertical land motion correction (to be subtracted from Mfil_upd)
% MRes = Sterodynamic residual (Mfil_upd-MVLM-MGRD-MIBE)

load('Budget_TimeSeries.mat')
[l,n] = size(Mfilc);

%% (2) PERFORM SSA WITH AN EMBEDDING DIMENSION OF 15 YEARS (ROUGHLY EQUAL)
% TO A CUTOFF PERIOD OF 30 YEARS. YOU NEED THE SSA TOOL FROM ASLAK
% GRINSTED (https://sites.google.com/a/glaciology.net/grinsted/software/ssatrend-m) 
% TO PERFORM THE ANALYSIS

for i = 1:n;
    progressbar(i/n)
    Mfilcsm(:,i) = ssatrend(Mfilc(:,i),180,std(detrend(Mfilc(:,i))));
end

%% (3) TEST THE SIGNIFICANCE OF THE NONLINEAR TREND AT EACH TIME STEP USING
% LONG-TERM CORRELATED NOISE WITH A HURST COEFFICIENT ALPHA = 0.8

% Load noise
load('LRD_Noise_USEastCoast.mat')

% Calculate standard deviation at each location to scale noise accordingly
for i = 1:n;
    progressbar(i/n);
    SD(i,1) = std(detrend(Mfilc(:,i)));
end

% Investigate the natural trends in the noise
num = 1000; % number of Monte-Carlo Simulations

X = [t ones(size(t))]; % Regression Matrix for linear trends
m = X\Mfilc;
yfit = X*m;
Tfilc = m(1,:)';
for i = 1:n;
    Noise1(:,:,i) = Noise.*SD(i)+repmat(yfit(:,i),1,num);
end

Noise_psm1 = Noise1;
for i = 1:n;
    i
    for j = 1:num;
        progressbar(j/num);
        Noise_psm1(:,j,i) = ssatrend(Noise1(:,j,i),180,1); % error has been chosen to be 1, as it does not matter for the experiment
    end
end
Noise_psmd = diff(Noise_psm1)*12;
Mfilcsmd = diff(Mfilcsm)*12;

% Calculate Probability that a rate in a given year exceeds the bounds of 
% natural variability plus its linear long-term trend to detect
% acceleration

for i = 1:n;
    progressbar(i/n)
    for j = 1:size(Noise_psmd,1);
        sP = find(Mfilcsmd(j,i)>abs(Noise_psmd(j,:,i)));
        P(j,i) = numel(sP)/num;
    end
end

[figure2] = Plot_RatesallStations(t,Mfilcsmd,Mds_upd,P,MID,n)