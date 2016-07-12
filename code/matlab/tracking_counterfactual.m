%tracking_counterfactual.m

clear
close all
directory = pwd;
Priority(2);
KbName('UnifyKeyNames');
% Screen('Preference', 'SkipSyncTests', 1)
screens = Screen('Screens'); %get screen numbers
screenNumber = max(screens);
setScreenDefaults %sets the screen defaults
commandwindow; %focus on command window

%-------------------------------------------------------------------------
%   DATA STRUCTURES
%-------------------------------------------------------------------------

prompt={'Participant:','Name:','Age:','Gender:'};
% Create all your text fields with the questions specified by the variable prompt.
title='Demographic information';
% The main title of your input dialog interface.
answer=inputdlg(prompt,title);
data.participant = answer{1};
data.name = answer{2};
data.age = answer{3};
data.gender = answer{4};

%-------------------------------------------------------------------------
%   DATA STRUCTURES
%-------------------------------------------------------------------------

% Eye-Tracking Data

n.t = 20;
dur.stim = 10;
n.iterations = 2;
n.maxfixations = 100;

exitflag = 0;

%%%Samples and event info, eye tracker-referenced times
etd = {}; %structure for saving eye-tracking data

etd.sr=1000;                                                                 %Eye tracker sampling rate (Hz)
etd.ssr=100; %change depending on size of data structure                                                                %Sample save rate (rate at which samples are saved to file, Hz)

etd.x = nan(etd.ssr*12,n.t,n.iterations);
etd.y = nan(etd.ssr*12,n.t,n.iterations);
etd.t = nan(etd.ssr*12,n.t,n.iterations);
etd.pa = nan(etd.ssr*12,n.t,n.iterations);

etd.fx = nan(n.maxfixations,n.t,n.iterations);
etd.fy = nan(n.maxfixations,n.t,n.iterations);
etd.fte = nan(n.maxfixations,n.t,n.iterations);
etd.fts = nan(n.maxfixations,n.t,n.iterations);
etd.fmt = nan(n.maxfixations,n.t,n.iterations);
etd.ff = nan(n.maxfixations,n.t,n.iterations);
etd.fpa = nan(n.maxfixations,n.t,n.iterations);

try
    %-------------------------------------------------------------------------
    %   ASSIGN MOVIES TO ARRAY
    %-------------------------------------------------------------------------
    
    %selection of movies
    randmovies = randperm(18);
    outcomes = [repmat(0,6,1)',reshape(repmat([0 1],3,1)',1,[]),repmat(1,6,1)',1,0];
    randmovies = [19, 20, randmovies]; %add practice clips
    
    %-------------------------------------------------------------------------
    %   EYE TRACKER SETUP
    %-------------------------------------------------------------------------
    
    %obs:           Observer identifier code number (1, 2, etc.)
    %               0 can be used for debugging purposes
    %rm:            Display on remote monitor? 0=no, 1=yes. Default=0
    %
    %dm:            Run in dummymode? 0=no, 1=yes. Default=0
    obs = 0;
    rm = 0;
    dm = 0;
    
    %monitor setup
    mon.wp=1024;                                                            %Monitor resolution, horizontal (pixels)
    mon.hp=768;                                                             %Monitor resolution, vertical (pixels)
    mon.wcm=38.4;                                                           %Width of viewable portion of monitor (cm; measure to be sure)
    mon.hcm=28.8;                                                           %Height of viewable portion of monitor (cm; measure to be sure)
    mon.dcm=59;                                                             %Distance from observer to monitor (cm; measure to be sure)
    mon.rf=85;                                                             %Monitor refresh rate (Hz)
    
    mon.pcm=mon.wcm/mon.wp;                                                     %Size of each pixel (cm)
    mon.pd=2*atan((mon.pcm/2)/mon.dcm)*180/pi;                                  %Size of each pixel (deg)
    mon.wd=mon.wp*mon.pd;                                                       %Width of monitor (deg)
    mon.hd=mon.hp*mon.pd;                                                       %Height of monitor (deg)
    mon.ed=4;                                                                   %Distance of fixation cross centers from monitor edge (degrees)
    mon.ep=round(mon.ed/mon.pd);                                                %Distance of fixation cross centers from monitor edge (pixels)
    AssertOpenGL;                                                               %Time is of the essence
    
    dres=SetResolution(rm,mon.wp,mon.hp);                                %Set monitor to given parameters and store original monitor settings for reset at end of script
    
    %-------------------------------------------------------
    %   OPEN WINDOW
    %-------------------------------------------------------------------------
    Screen('Preference', 'SkipSyncTests', 1);
    [win,rect]=Screen('OpenWindow', 0, [128,128,128]);
    scx=rect(3);                        % screen width
    scy=rect(4);                       % screen height
    Screen('TextFont',win,'Arial'); %text/font
    x0=scx/2;                              % for fixation rectangle drawing, define middle coords.
    y0=scy/2;                              % for fixation rectangle drawing, define middle coords.
    
    mon.slack=Screen('GetFlipInterval',win)/2;
    
    %%%Various PsychToolbox, eye tracker settings and misc
    HideCursor;
    [el,edfFile,eter]=setEyetrackerDefaults(win,data.participant,mon,dm);                    %My own script that sets up the EyelinkToolbox environment
    if eter == 1                                                                %Something messed up with the link to the eye tracker
        return;
    end
    EyelinkDoTrackerSetup(el);                                                  %Go to main eye tracker screen (calibration, thresholding, etc.)
    [et.cal,tCal]=Eyelink('CalMessage');                                        %Retrieve calibration results
    if ~isempty(tCal)
        Screen('TextSize',win,30);
        trCal=Screen('TextBounds',win,tCal);
        Screen('DrawText',win,tCal,(mon.wp-trCal(3))/2,(mon.hp+trCal(4))/2,0,[],1);
        Screen('Flip',win);                                                         %Display calibration results
    end
        
    % -------------------------------------------------------------------------
    %   INSTRUCTIONS
    % -------------------------------------------------------------------------
    
    % GIVE INSTRUCTIONS
    ShowCursor;
    instrimage = imread('instructions_counterfactual.jpg','jpg');
    Screen('PutImage',win,instrimage,[x0-512,y0-384,x0+512,y0+384]);
    t.instr_1 = Screen('Flip', win);
    
    %Wait for mouse click
    [~,~,~,t.instr_1_click] = getMouseClick([0 scx],[0 scy]);
        
    % -------------------------------------------------------------------------
    %   CLIP NUMBER
    % -------------------------------------------------------------------------
    for     k = 1:length(randmovies);       
        % Clip number
        Screen('TextSize',win,120);
        DrawFormattedText(win,['Clip ',num2str(k)],'center', 'center');
        t.clipnr(k) = Screen('Flip', win);
        WaitSecs(3);
        
        proceedflag = 0;
        while(~proceedflag)
            
            % Fixation cross
            ShowCursor;
            Screen('DrawLine',win,[0 0 0], x0-20, y0,x0+20,y0,5)
            Screen('DrawLine',win,[0 0 0], x0, y0-20,x0,y0+20,5)
            t.fixation_first(k) = Screen('Flip', win);
            [x, y, response, t.fixation_first_click(k), exitflag] = getMouseClick([x0-20 x0+20],[y0-20 y0+20]);
            HideCursor;
            if (exitflag == 1)
                break;
            elseif (exitflag == 2)
                EyelinkDoTrackerSetup(el);
            else
                proceedflag = 1;
            end
        end
        
        if (exitflag == 1)
            break;
        end
                
        WaitSecs(1+rand(1));
        
        %--------------------------------------------------------------------------
        %   LOAD VIDEO
        %--------------------------------------------------------------------------
        for iteration= 1:2
            
            % Open movie file and retrieve basic info about movie:
            moviename= ['../../videos/clips/actual/clip_',num2str(randmovies(k)),'.mov']; %directory needs to be adapted 
            [et, t.mti{k,iteration}, t.ti{k,iteration}, t.ft{k,iteration}] ...
                = PlayMovie(moviename, win, el);            
       
            %save eye-tracking data
            i = randmovies(k);
            
            etd.x(1:length(et.x),i,iteration) = et.x;
            etd.y(1:length(et.y),i,iteration) = et.y;
            etd.t(1:length(et.t),i,iteration) = et.t;
            etd.pa(1:length(et.pa),i,iteration) = et.pa;
            
            etd.fx(1:length(et.fx),i,iteration) = et.fx;
            etd.fy(1:length(et.fy),i,iteration) = et.fy;
            etd.fte(1:length(et.fte),i,iteration) = et.fte;
            etd.fts(1:length(et.fts),i,iteration) = et.fts;
            etd.fmt(1:length(et.fmt),i,iteration) = et.fmt;
            etd.ff(1:length(et.ff),i,iteration) = et.ff;
            etd.fpa(1:length(et.fpa),i,iteration)=et.fpa;
            
            %displays text
            Screen('TextSize',win,30);
            if iteration == 1
                DrawFormattedText(win, 'You will now see the same clip once more', 'center', 'center', [0 0 0]);
                t.clipagain(k) = Screen('Flip',win);
                WaitSecs(3);
                proceedflag = 0;
                while(~proceedflag)
                    % Fixation cross
                    ShowCursor; %Show mouse cursor
                    Screen('DrawLine',win,[0 0 0], x0-20, y0,x0+20,y0,5)
                    Screen('DrawLine',win,[0 0 0], x0, y0-20,x0,y0+20,5)
                    t.fixation_second(k) = Screen('Flip', win);
                    [x, y, response, t.fixation_second_click(k), exitflag] = getMouseClick([x0-20 x0+20],[y0-20 y0+20]);
                    HideCursor;
                    if (exitflag == 1)
                        break;
                    elseif (exitflag == 2)
                        EyelinkDoTrackerSetup(el);
                    else
                        proceedflag = 1;
                    end
                end
                if (exitflag == 1)
                    break;
                end
            else
                ShowCursor; %Show mouse cursor
                imagename = 'counterfactual.jpg';       
                instrimage = imread(imagename,'jpg');
                Screen('PutImage',win,instrimage,[x0-512,y0-384,x0+512,y0+384]);
                t.response_screen(k) = Screen('Flip',win,0,1);                
                [x, y, ~, t.response_click(k)] = getMouseClick([x0-300 x0+300], [y0-20, y0+20]);
                                
                Screen('FillOval', win, [0 0 0], [x-10 y-10 x+10 y+10]);
                
                % RECORD DATA
                data.trial(k) = k; %trial number
                data.movie(k) = i; %movie number
                data.outcome(k) = outcomes(i); %outcome
                data.rating(k) = x; %rating
                t.response_dot(k) = Screen('Flip',win,0);
                WaitSecs(1);
            end
            if (exitflag == 1)
                break;
            end
        end
        
    end %clips
    
    %--------------------------------------------------------------------------
    %   END SCREEN
    %--------------------------------------------------------------------------
    Screen('TextSize',win,50);
    DrawFormattedText(win, 'Thanks!', 'center', 'center', [0 0 0]);
    DrawFormattedText(win, 'You''ve completed the experiment.', 'center', y0+50, [0 0 0]);
    t.final_screen = Screen('Flip', win);
    
    KbWait;
    Eyelink('CloseFile');                                                       %Close EDF file
    % Close Screen, we're done:
    
    %save eye-tracker data
    try
        Eyelink('ReceiveFile');
        % movefile(edfFile,'/location/to/save/the/file');
    catch edferr
        fprintf('Problem receiving data file ''%s''\n',edfFile);
        disp(edferr);
    end
    
    %save matlab data
    % save(['/location/to/save/the/file' num2str(data.participant) ...
    %     '.mat'],'data','etd','t','mon');
    
    Eyelink('Shutdown');
    %Close link with eye tracker
    restoreScreenDefaults;                                                      %My script that returns environment to defaults
    Screen('CloseAll');
catch
    sca;
    Screen('CloseAll');
    keyboard
    rethrow(psychlasterror);
end



