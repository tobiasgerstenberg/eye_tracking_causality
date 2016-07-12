function [et,mti,ti,ft] = PlayMovie(moviename, win, el)
a = Screen('OpenMovie', win, moviename);
mti=[];ti=[];ft=[];
% Seek to start of movie (timeindex 0):
Screen('SetMovieTimeIndex', a, 0);

%play movie
Screen('PlayMovie',a,1);
WaitSecs(.1);                                                               %Loads MEX files to avoid delays with future use
Eyelink('StartRecording');                                                  %Start receiving eye tracker samples

%initialize eye-tracking variables for fixations 
et.fx = nan; 
et.fy = nan; 
et.fte = nan; 
et.fts = nan; 
et.fmt = nan; 
et.ff = nan; 
et.fpa = nan; 

%variables for eye-tracking
f=1;
nf=1;

% Infinite playback loop: Fetch video frames and display them...
while(1)
    [mt,mti(f)]=Screen('GetMovieImage',win,a);
    % Valid texture returned?
    if mt<=0
        break;
    end;    
    if mt > 0
        ti(f)=Screen('GetMovieTimeIndex',a);
        Screen('DrawTexture',win,mt);
        ft(f)=Screen('Flip',win);
        
        %%%Monitor for fixation-related events
        [sdata,edata]=Eyelink('GetQueuedData');
        et.samps{f}=sdata;
        et.evts{f}=edata;
        if ~isempty(edata) && any(edata(2,:) == el.ENDFIX)
            en=find(edata(2,:) == el.ENDFIX);
            et.fx(nf)=edata(19,en);
            et.fy(nf)=edata(20,en);
            et.fpa(nf)=edata(21,en);
            et.fts(nf)=edata(5,en);
            et.fte(nf)=edata(6,en);
            et.fmt(nf)=Screen('GetMovieTimeIndex',a);
            et.ff(nf)=f;
            nf=nf+1;
        end
        
        if ~isempty(sdata)
            gss=sdata(14,:)>0;
            et.x(f)=nanmean(sdata(14,gss));
            et.y(f)=nanmean(sdata(16,gss));
            et.t(f)=nanmean(sdata(1,gss));
            et.pa(f)=nanmean(sdata(12,gss));
        end
                
        Screen('Close',mt);
        
    end
    f=f+1;
end

Eyelink('StopRecording');                                                  %Start receiving eye tracker samples

% Done. Stop playback:
Screen('PlayMovie', a, 0);
 
% Close movie object:
Screen('CloseMovie', a);

