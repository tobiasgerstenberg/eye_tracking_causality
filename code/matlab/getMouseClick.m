function [x,y,r,t,exitflag]=getMouseClick(xok,yok)
%
%[x,y,r,t]=getMouseClick(xok,yok)
%
%This function monitors for mouseclicks to specific parts of the screen. It
%then outputs the (x,y) position of a valid click, a response, r,
%corresponding to that location, and the time, relative to the computer's
%clock, for the selection.
%
%OUTPUT VARIABLES
%x:     x-position, in pixels, of a valid click.
%y:     y-position, in pixels, of a valid click.
%r:     Response variable. This depends on how many possible selections
%       have been specified in the xok and yok input variable.
%t:     Time of the valid click. This is relative to the computer clock,
%       analagous to using the GetSecs command.
%
%INPUT VARIABLES
%xok:   An (n by 2) matrix specifying the left and right x-coordinates for
%       valid response regions.
%yok:   An (n by 2) matrix specifying the upper and lower y-coordinates for
%       valid response regions.
%
%For instance... an input might be:

%xok = [100 200          yok = [250 350
%       350 450                 250 350
%       600 700]                250 350]
%
%The valid response regions would be three squares, each 100 pixels by 100
%pixels, aligned along the horizontal midline of an 800x600 monitor.

exitflag=0;
validresp=0;                                                                %Only clicks within the specified regions will be accepted
while ~validresp
    [x,y,buttons]=GetMouse;
    while any(buttons)                                                      %If the mouse is already clicked when we start the loop, we wait until it is unclicked
        [x,y,buttons]=GetMouse;
    end
    while ~any(buttons)                                                     %Now we wait until it is clicked
        [keyIsDown,~,keyCode]=KbCheck;
        if keyIsDown
            if keyCode(KbName('q'))
                exitflag=1;
                break;
            elseif keyCode(KbName('c'))
                exitflag=2;
                break;
            end
        end
        [x,y,buttons]=GetMouse;
    end
    while any(buttons)                                                      %And again, wait until the mouse is unclicked again to record the final position
        [x,y,buttons]=GetMouse;
    end
    if exitflag ~= 0
        x=nan;
        y=nan;
        r=nan;
        t=nan;
        break;
    else
        for i=1:size(xok,1)                                                     %Go through each possible response region
            if (y > yok(i,1) && y < yok(i,2) && x > xok(i,1) && x < xok(i,2))   %If the click is within the region...
                r=i;                                                            %Record that response
                validresp=1;                                                    %The response is valid, so allow to break out of the loop
                t=GetSecs;                                                      %Record the timing of the valid response
            end
        end
    end
end