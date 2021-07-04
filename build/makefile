default:
	ghc ../src/Main.hs ../src/Ray.hs ../src/Vec3.hs ../src/Types.hs -O3 -o raytracer
	rm -rf ../src/*.hi
	rm -rf ../src/*.o

clean:
	rm -rf raytracer image.ppm

run:
	make
	./raytracer > image.ppm