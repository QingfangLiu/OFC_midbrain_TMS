
clc; clear;
maskpath = '/Users/qingfangliu/Desktop/GlobalConnectedness/masks';

% load extracted and filtered scan data
load('tc_filtered_3mm.mat'); 
hdr = spm_vol(fullfile(maskpath,'gm_0.1_3mm.nii'));
gm = spm_read_vols(hdr);
gm_idx = find(gm > 0);
nvox = length(gm_idx); % number of voxels
nscans = 430; % number of scans in each run

%%
nhdr = spm_vol(fullfile(maskpath,'wm_0.9_3mm.nii'));
nhdr.fname = 'out.nii';

dat = zscore(dat); % zcore for convenient Pearson correlation calculation
cors = zeros(1,nvox); 

parfor i = 1:nvox
    tmp = (dat(:,i)' * dat)./ nscans; % Pearson correlation on z-scored values (with each voxel)
    cors(i) = sum(abs(atanh(tmp(tmp<1))))/nvox; 

    if ~mod(i,5000)
        fprintf('\t\t%d of %d\n', i, nvox);
    end

end

vol = zeros(hdr.dim);
vol(gm_idx) = cors;
spm_write_vol(nhdr, vol); 