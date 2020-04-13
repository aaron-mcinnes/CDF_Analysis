% SCM+ 

participants = repmat(1:10, 1, 5).';
%%

cond = repelem("BB_Flex", 50).';
latency = normrnd(104, 7, length(cond), 1); %mu, sd, nrow, ncol

BB_Flex = [participants, cond, latency];


%%

cond = repelem("FDI_Pinch", 50).';
latency = normrnd(143, 11, length(cond), 1); %mu, sd, nrow, ncol

FDI_Pinch = [participants, cond, latency];

%%

cond = repelem("BB_PinchFlex", 50).';
latency = normrnd(109, 9, length(cond), 1); %mu, sd, nrow, ncol

BB_PinchFlex = [participants, cond, latency];

%%

cond = repelem("FDI_PinchFlex", 50).';
latency = normrnd(120, 8, length(cond), 1); %mu, sd, nrow, ncol

FDI_PinchFlex = [participants, cond, latency];

%%

SCMPlus = [BB_Flex; FDI_Pinch; BB_PinchFlex; FDI_PinchFlex];
SCM = repelem("SCM+", length(SCMPlus)).';
SCMPlus = [SCM, SCMPlus];

%% SCM- 
%%

cond = repelem("BB_Flex", 50).';
latency = normrnd(144, 10, length(cond), 1); %mu, sd, nrow, ncol

BB_Flex = [participants, cond, latency];

%%

cond = repelem("FDI_Pinch", 50).';
latency = normrnd(159, 12, length(cond), 1); %mu, sd, nrow, ncol

FDI_Pinch = [participants, cond, latency];

%%

cond = repelem("BB_PinchFlex", 50).';
latency = normrnd(165, 12, length(cond), 1); %mu, sd, nrow, ncol

BB_PinchFlex = [participants, cond, latency];

%%

cond = repelem("FDI_PinchFlex", 50).';
latency = normrnd(181, 13, length(cond), 1); %mu, sd, nrow, ncol

FDI_PinchFlex = [participants, cond, latency];

%%

SCMMinus = [BB_Flex; FDI_Pinch; BB_PinchFlex; FDI_PinchFlex];
SCM = repelem("SCM-", length(SCMMinus)).';
SCMMinus = [SCM, SCMMinus];

%%
data = [SCMMinus; SCMPlus];

cd('/Users/aaronmcinnes/ODcurtin/2020/CDF_Analysis/CDF_DataSimulation/SimulatedData/')
save('SimulatedData', 'data')

data = array2table(data, 'VariableNames',{'SCM', 'Subject', 'Cond','Latency'});
writetable(data, 'SimulatedData.xlsx')
