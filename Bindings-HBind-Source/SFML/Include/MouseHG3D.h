// MouseHG3D - special mouse class to get a specialized way to obtain
// the mouse coordinates

#include <SFML/System/Vector2.hpp>
#include <SFML/Window/Mouse.hpp>

class MouseHG3D : sf::Mouse {
	
	public:
		static void getPosition(int &x, int &y) {
			sf::Vector2i vi = sf::Vector2i();
			vi = sf::Mouse::getPosition();
			x = vi.x;
			y = vi.y;
		};
	
};

