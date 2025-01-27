import pycharge as pc
from numpy import linspace, meshgrid
from scipy.constants import e
sources = (pc.StationaryCharge((10e-9, 0, 0), e),
           pc.StationaryCharge((-10e-9, 0, 0), -e))
simulation = pc.Simulation(sources)
coord = linspace(-50e-9, 50e-9, 1001)
x, y, z = meshgrid(coord, coord, 0, indexing='ij')
Ex, Ey, Ez = simulation.calculate_E(0, x, y, z)
V = simulation.calculate_V(0, x, y, z)