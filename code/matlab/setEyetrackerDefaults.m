function [el,edfFile,eter]=setEyetrackerDefaults(win,obs,mon,dm)

eter=0;
if ~EyelinkInit(dm,1)
    Screen('CloseAll');
    disp('Cannot initilaize eye tracker');
    eter=1;
    return;
end
% x=mon.wcm*10/2;
% y=mon.hcm*10/2;
el=EyelinkInitDefaults(win);
edfFile=['et_' num2str(obs) '.edf'];
Eyelink('OpenFile',edfFile);
el.backgroundcolour=128;
el.foregroundcolour=0;
Eyelink('command','screen_pixel_coords = %ld %ld %ld %ld',0,0,mon.wp-1,mon.hp-1);
Eyelink('message','DISPLAY_PIXEL_COORDS %ld %ld %ld %ld',0,0,mon.wp-1,mon.hp-1);
% Eyelink('command','screen_phys_coords = %ld %ld %ld %ld',-x,y,x,-y);
% Eyelink('message','DISPLAY_PHYSICAL_COORDS %ld %ld %ld %ld',-x,y,x,-y);
% Eyelink('command','screen_distance = %ld %ld %ld',10*mon.dccm,10*mon.dtcm,10*mon.dbcm);
% Eyelink('message','DISPLAY_DISTANCE_TO_SCREEN %ld %ld %ld',10*mon.dccm,10*mon.dtcm,10*mon.dbcm);
% Eyelink('command','calibration_type = HV9');
Eyelink('command','enable_automatic_calibration = YES');
Eyelink('command','automatic_calibration_pacing = 1000');
Eyelink('command','saccade_velocity_threshold = 35');
Eyelink('command','saccade_acceleration_threshold = 9500');
Eyelink('command','file_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON');
Eyelink('command','file_sample_data  = LEFT,RIGHT,GAZE,HREF,AREA,GAZERES,STATUS');
% Eyelink('command','link_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON');
Eyelink('command','link_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON');
Eyelink('command','link_sample_data  = LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS');