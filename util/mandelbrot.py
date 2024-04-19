import time

def init_pos(x: float, y: float):
    self = Pos()
    self.x = x
    self.y = y
    return self
    
class Pos:
    x: float
    y: float
        
    def add(self, b):
        return init_pos(self.x + b.x,self.y + b.y)
    def sub(self, b):
        return init_pos(self.x - b.x,self.y - b.y)
    def mul(self, b):
        return init_pos(self.x * b.x,self.y * b.y)
    def div(self, b):
        return init_pos(self.x / b.x,self.y / b.y)
    
def mandelbrot(c: Pos, z: Pos, steps: int):  
    i = 0
    zSq = z.mul(z)
    while i < steps and zSq.x + zSq.y < 4.0:
        z.y = z.x * 2.0 * z.y
        z.x = zSq.x - zSq.y
        z = z.add(c)
        zSq = z.mul(z)
        i = i + 1
    
    return i


def timestamp(): 
    return time.time()
    
def main():
    start = timestamp();
    max_steps = 45
    width = 70
    height = 35
    x_speed = 0.03
    y_speed = 0.06
    x_start = 0.0 - 1.5
    y_start = 0.0 - 1.0

    out = ""
    pos = init_pos(x_start, y_start)
    for _ in range(0, height):
        for _ in range(0, width):
            steps = mandelbrot(pos, init_pos(0.0, 0.0), max_steps)
            if steps == max_steps:
                out += '@'
            else:
                out += ' '
            
            pos.x = pos.x + x_speed
        
        out += '|'
        out += '\n'
        pos.x = x_start
        pos.y = pos.y + y_speed
    
    print(out)
    end = timestamp()
    print("Finished running main() in {} seconds.".format((end - start).__round__(5)))

main()