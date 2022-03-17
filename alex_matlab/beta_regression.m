%Root folder
close all 
clearvars
RootFolder = '..\prelim_results\';
%Files = {'beta_dist_10.csv', 'beta_dist_50.csv', 'beta_dist_100.csv'};
Files = {'beta_dist_100.csv'};

FN =[ RootFolder, Files{1}];

tData = readtable(FN);
%Create a table for results
Names = ["parameter_id", "timeSeriesID", "slope", "" "intercept"];
Types = ["string",             "string", "double", "double"];

tRes = table('Size', [length(lParameter_id) length(Names)], 'VariableNames', Names, 
                        'VariableTypes', Types);

                        %load paramters 
                        %for each param ID

beta_dist_100 %>% 
  mutate(temp_dist = YEAR2 - YEAR1) %>% 
  filter(parameter_id == 1 & timeSeriesID=='ts1') %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  # filter(YEAR1==min(YEAR1)) %>% 
  # , 'ts2', 'ts3', 'ts4', 'ts5', 'ts6', 'ts7', 'ts8', 'ts9'
  ggplot() +
  # facet_wrap(~parameter_id, scales = 'free_x') +
  geom_point(aes(x = c_temp_dist, y = Jbeta, colour = timeSeriesID),
             # size = 0.5, alpha = 0.2
             ) +
  stat_smooth(aes(x = c_temp_dist, y = Jbeta, 
                  colour = interaction(parameter_id, timeSeriesID)),
              method = 'lm', se = F) +
  theme(legend.position = 'none')


beta_dist_100 %>% 
  filter(parameter_id %in% c(1:9) & timeSeriesID %in% c('ts2')) %>% 
  # , 'ts2', 'ts3', 'ts4', 'ts5', 'ts6', 'ts7', 'ts8', 'ts9'
  ggplot() +
  facet_wrap(~parameter_id, scales = 'free_x') +
  geom_point(aes(x = YEAR2, y = Jbeta, colour = timeSeriesID),
             size = 0.5, alpha = 0.2) +
  stat_smooth(aes(x = YEAR2, y = Jbeta, 
                  colour = interaction(parameter_id, timeSeriesID)),
              method = 'lm', se = F) +
  theme(legend.position = 'none')

  
  
  
  
  ;

FN = [RootFolder sFile];
% file

lParameter_id = unique(t.parameter_id);

%for each file
for iP = 1:length(lParameter_id)
    %Get pres/ab data
    tSubSet = t(t.parameter_id==lParameter_id(iP), :);
    tSubSetUnst = unstack(tSubSet, 'n', 'species');
    tSubSetUnst = fillmissing(tSubSetUnst,'constant',0);
    SpAbund = tSubSetUnst{:, 3:end};
    SpAbund = SpAbund >0;%get presence absence 
    spDiff = diff( SpAbund, 1, 1);
    spSum = ( SpAbund(1:end-1, :) +  SpAbund(2:end, :));
    spSum(spSum(:) < 2)=0;
    spTrans = spDiff + spSum;
    [Ts, Grps] = grpstats(spTrans(:), spTrans(:),{ 'numel', 'gname'}) ;
    %-1, T10, 0, T00, 1, T01, 2, T11
    Ind = str2num(char(Grps{:})) + 2;
    Ts = Ts(Ind);
    
    Ts = Ts(Ind([2, 3, 4, 1]));
    Ts([1:2]) = Ts([1:2])/sum(Ts([1:2]));
    Ts([3:4]) = Ts(3:4)/sum(Ts([3:4]));
    
    %get differences
    tRes(iP, {'parameter_id', 'T00', 'T01',  'T11', 'T10'}) = {lParameter_id(iP), Ts(1), Ts(2), Ts(3), Ts(4)};
    %get sum,)
    %set sum to 0 where sum <2
    %diffrnc = d+s  (-1, 0, 1, 2)
    %find the number of transitions
end
%find prbobabilities
[filepath,name,ext] = fileparts(sFile);
ResFN = [RootFolder name, '_Ts.', ext];
writetable(tRes, ResFN)

