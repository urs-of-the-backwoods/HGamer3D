#include "enet/enet.h"
#include "deque"
#include "map"

using namespace std;

class EnetPacket
{
	ENetPacket* m_packet;
	char m_peer[128];
	int m_channel;
	EnetPacket(ENetPacket* packet, ENetPeer* peer, int channel);

public:
	char* getData();
	char* getPeer();
	int getChannel();
	~EnetPacket();
	friend class EnetServer;
	friend class EnetClient;
};


class EnetServer
{
	deque<EnetPacket*> m_packets;
	map<string,ENetPeer*> m_peers; 
	ENetHost *m_server;
	EnetServer(ENetHost* server);
public:
	~EnetServer();
	bool serve(int msWait);
	EnetPacket* getPacket();
	void send(const char* peer, const char* message, int channel);
	friend class HG3DEnet;
};

class EnetClient
{
	ENetHost *m_client;
	ENetPeer *m_peer;
	EnetClient(ENetHost* client);
	deque<EnetPacket*> m_packets;
public:
	~EnetClient();
	bool connect(const char* server, int port);
	bool disconnect();
	bool serve(int msWait);
	EnetPacket* getPacket();
	void send(const char* message, int channel);
	friend class HG3DEnet;
};



class HG3DEnet
{
public:
	HG3DEnet();
	~HG3DEnet();
	static EnetServer* createServer(int port);	
	static EnetClient* createClient();
};
