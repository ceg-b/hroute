clear;
delta=.01;
speed=.1;
[xx,yy]=meshgrid([-2:.1:4],[-2:.1:4]);

fun = @(xx,yy) exp(-(xx.^2+yy.^2))+.5*exp(-((xx-1.0).^2+(yy-2.5).^2));

surf(xx,yy,fun(xx,yy))
shading interp


start_point=[4,1];
start_val=fun(start_point(1),start_point(2));
track=[start_point,start_val];

do

  d_dx=1/delta*(fun(start_point(1)+delta,start_point(2))-start_val);
  d_dy=1/delta*(fun(start_point(1),start_point(2)+delta)-start_val);
  shift=[d_dx,d_dy];
  shift=shift/norm(shift)
  start_point=start_point+shift*speed;
  start_val=fun(start_point(1),start_point(2));
  track=[track;[start_point,start_val]];
until (track(end)-track(end-1,end)<0)

#track=track(1:end-1,:);
hold on

plot3(track(:,1),track(:,2),track(:,3)+.01,'linewidth',4,'color',[0,0,0])

