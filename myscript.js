const w = 800;
const h = 400;
const datalen = 10;
let svg = d3.select("svg")
  .attr("width",w)
  .attr("height",h);

let background = svg.append("rect")
  .attr("class","background")
  .attr("width",w)
  .attr("height",h);

let bands = svg.append("g")

bands.selectAll("rect.band")
  .data(_.range(datalen))
  .enter()
  .append("rect")
  .attr("height",h)
  .attr("width",w/datalen)
  .attr("class","band")
  .attr("x",d=>d*w/datalen);

let line = d3.line()
  .x(d=>d[0])
  .y(d=>d[1]);

let pathdata = {};
let path = svg.append("path")
  .attr("class","yourpath");

background
  .on("mousedown",()=>{
    background
      .on("mousemove",function(d,i){
        position = Math.round(d3.event.offsetX/(w/datalen));
        pathdata[position] = [position*w/datalen,d3.mouse(this)[1]];
        path.datum(_.values(pathdata)).attr("d",line);
      })
      .on("mouseup",()=>{
        background
          .on("mousemove",null)
          .on("mouseup",null);
      });
  });