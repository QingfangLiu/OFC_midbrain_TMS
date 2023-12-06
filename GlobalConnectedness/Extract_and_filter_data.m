
clc; clear;

% what is not provided in the code
% 'path': where the functional images are saved
% 'mreg': matrix of nuisance regressors (# rows equals to # scans)

maskpath = '~/Desktop/GlobalConnectedness/masks';
nscans = 430; % number of scans in each run

gm_nii = fullfile(maskpath,'gm_0.1_3mm.nii'); % gray matter mask
gm_dat = spm_read_vols(spm_vol(gm_nii));
gm_idx = find(gm_dat > 0);
nvox = length(gm_idx); % number of voxels

wm_nii = fullfile(maskpath,'wm_0.9_3mm.nii'); % white matter mask
wm_dat = spm_read_vols(spm_vol(wm_nii));
wm_idx = find(wm_dat > 0);

csf_nii = fullfile(maskpath,'csf_0.9_3mm.nii'); % CSF mask
csf_dat = spm_read_vols(spm_vol(csf_nii));
csf_idx = find(csf_dat > 0);

%%
dat = zeros(nscans,nvox); % initiliaze to get data
wm_mean = zeros(nscans,1);
csf_mean = zeros(nscans,1);

% load data
% functional scans (of a run) are saved in the 'path' folder
% then we load data written in 3mm voxel size and 6mm smoothing
n = dir(fullfile(path, 's6mmw3f*.nii')); 

for i = 1:nscans
    tmp = spm_read_vols(spm_vol(fullfile(path, n(i).name)));
    dat(i,:) = tmp(gm_idx); % gray matter

    wm_mean(i) = mean(tmp(wm_idx)); % white matter: take the mean
    csf_mean(i) = mean(tmp(csf_idx)); % CSF: take the mean
end

% filter data
% 'mreg' is the nuisance regressor with size of nscans * (# nuisance regressors)
mreg = [zscore([mreg, mean(dat,2), wm_mean, csf_mean, [1:nscans]']), ones(nscans,1)]; % add mean(gm), mean(wm), mean(csf), drift, and constant
b = inv(mreg'*mreg)*(mreg'*dat);
dat = dat - mreg*b;

% save the filtered data (in the same 'path' folder or somewhere else)
save(fullfile(path,'tc_filtered_3mm.mat'), 'dat');       
       
    
