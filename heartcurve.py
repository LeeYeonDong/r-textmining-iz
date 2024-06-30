import numpy as np
import matplotlib
from matplotlib import pyplot as plt
from matplotlib.animation import FuncAnimation

# Use the TkAgg backend
matplotlib.use('TkAgg')

# Define the heart curve function
def heart_curve(t):
    x = 16 * np.sin(t)**3
    y = 13 * np.cos(t) - 5 * np.cos(2*t) - 2 * np.cos(3*t) - np.cos(4*t)
    return x, y

# Set up the figure, add title, and make the line thicker
fig, ax = plt.subplots()
x, y = heart_curve(0)
line, = ax.plot(x, y, color='pink', linewidth=10)
ax.set_xlim(-20, 20)
ax.set_ylim(-20, 20)
ax.set_title("congratulations on our 100th day♡", fontsize=50)

# Initialize the animation with an empty line
def init():
    line.set_data([], [])
    return line,

# Update function for the animation
def update(frame):
    t = np.linspace(0, frame, 5000)
    x, y = heart_curve(t)
    line.set_data((x, y))
    return line,

# Create the animation
ani = FuncAnimation(fig, update, frames=np.linspace(0, 2*np.pi, 50), init_func=init, blit=True)

plt.show()  # This will display the animation in a separate window
ani.save('D:/대학원/heart_curve_animation.gif', writer='pillow', fps=60)