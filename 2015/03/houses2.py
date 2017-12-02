def day03(data):
  def solve(moves, house=(0,0)):
    move = lambda (x,y), d: {'^':(x,y+1),'v':(x,y-1),'<':(x-1,y),'>':(x+1,y)}[d]
    houses = set([house])
    for direction in moves:
      house = move(house, direction)
      houses.add(house)
    return houses
  print("Part 1: {0} different houses".format(len(solve(data))))
  print("Part 2: {0} different houses".format(len(solve(data[::2]) | solve(data[1::2]))))

if __name__ == '__main__':
    with open('input') as f:
        data = f.read().strip()
    day03(data)
