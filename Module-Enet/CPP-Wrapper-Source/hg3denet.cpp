#include "enet/enet.h"
#include "hg3denet.hpp"

#include "deque"
#include "map"
#include "string"
using namespace std;

// EnetPacket - for receiving (not sending) data
//

EnetPacket::EnetPacket(ENetPacket* packet, ENetPeer* peer, int channel)
{
	m_packet = packet;
	sprintf(m_peer, "%x:%u", peer->address.host, peer->address.port);
	m_packet->data[m_packet->dataLength-1]=0;
	m_channel = channel;
};

char* EnetPacket::getData()
{
	return (char*) m_packet->data;
};

char* EnetPacket::getPeer()
{
	return m_peer;
};

int EnetPacket::getChannel()
{
	return m_channel;
};


EnetPacket::~EnetPacket()
{
	enet_packet_destroy(m_packet);
};


// EnetServer - the server, can hold multiple connections from clients
//

EnetServer::EnetServer(ENetHost* server)
{
	m_server = server;
};

EnetServer::~EnetServer()
{
	enet_host_destroy(m_server);
};
	
bool EnetServer::serve(int msWait)
{
	ENetEvent event;
	char buff[128];

	/* Wait up to 1000 milliseconds for an event. */
	if (enet_host_service (m_server, & event, msWait) > 0)
	{
		switch (event.type)
		{
		case ENET_EVENT_TYPE_CONNECT:
			sprintf(buff, "%x:%u", event.peer->address.host, event.peer->address.port);
			m_peers[string(buff)] = event.peer;
			break;

		case ENET_EVENT_TYPE_RECEIVE:
			//push packet to queue
			m_packets.push_front(new EnetPacket(event.packet, event.peer, event.channelID));
			break;
       
		case ENET_EVENT_TYPE_DISCONNECT:
			sprintf(buff, "%x:%u", event.peer->address.host, event.peer->address.port);
			m_peers.erase(string(buff));
			event.peer -> data = NULL;
		};
		return true;
	}
	return false;
};

EnetPacket* EnetServer::getPacket()
{
	if (m_packets.empty())
	{
		return NULL;
	}
	else
	{
		EnetPacket* p = m_packets.back();
		m_packets.pop_back();
		return p;
	}
};

void EnetServer::send(const char* peerid, const char* message, int channel)
{
	/* Create a reliable packet of size containing "message\0" */
	ENetPacket * packet = enet_packet_create (message, 
											  strlen (message) + 1, 
											  ENET_PACKET_FLAG_RELIABLE);

	/* find the peer */
	ENetPeer* peer = m_peers[peerid];
	if (peer != NULL)
		enet_peer_send (peer, channel, packet);

	/* One could just use enet_host_service() instead. */
	enet_host_flush (m_server);
};


// and here comes the client
//

bool EnetClient::connect(const char* server, int port)
{
	ENetAddress address;
	ENetPeer* peer;
	ENetEvent event;

	if (m_peer == NULL)
	{
		/* Connect to some.server.net:1234. */
		enet_address_set_host (& address, server);
		address.port = port;
		/* Initiate the connection, allocating the three channels 0, 1 and 2. */
		peer = enet_host_connect (m_client, & address, 3, 0);    
		if (peer != NULL)
		{
			if (enet_host_service (m_client, & event, 5000) > 0 &&
				event.type == ENET_EVENT_TYPE_CONNECT)
			{
				m_peer = peer;
				return true;
			}
			else
			{
				enet_peer_reset (peer);
			}
		}
	};
	return false;
};

bool EnetClient::disconnect()
{
	ENetEvent event;
	if (m_peer != NULL)
	{
		enet_peer_disconnect (m_peer, 0);
		/* Allow up to 3 seconds for the disconnect to succeed
		   and drop any packets received packets.
		 */
		while (enet_host_service (m_client, & event, 3000) > 0)
		{
			switch (event.type)
			{
			case ENET_EVENT_TYPE_RECEIVE:
				enet_packet_destroy (event.packet);
				break;
			case ENET_EVENT_TYPE_DISCONNECT:
				return true;
			}
		}
		/* We've arrived here, so the disconnect attempt didn't */
		/* succeed yet.  Force the connection down.             */
		enet_peer_reset (m_peer);
	}
	m_peer = NULL;
	return false;
};

bool EnetClient::serve(int msWait)
{
	ENetEvent event;

	/* Wait up to 1000 milliseconds for an event. */
	if (enet_host_service (m_client, & event, msWait) > 0)
	{
		switch (event.type)
		{

		case ENET_EVENT_TYPE_RECEIVE:
			//push packet to queue
			m_packets.push_front(new EnetPacket(event.packet, event.peer, event.channelID));
			break;
       
		};
		return true;
	}
	return false;
};

EnetPacket* EnetClient::getPacket()
{
	if (m_packets.empty())
	{
		return NULL;
	}
	else
	{
		EnetPacket* p = m_packets.back();
		m_packets.pop_back();
		return p;
	}
};

void EnetClient::send(const char* message, int channel)
{
	if (m_peer != NULL)
	{
		/* Create a reliable packet of size containing "message\0" */
		ENetPacket * packet = enet_packet_create (message, 
												  strlen (message) + 1, 
												  ENET_PACKET_FLAG_RELIABLE);

		enet_peer_send (m_peer, channel, packet);

		/* One could just use enet_host_service() instead. */
		enet_host_flush (m_client);
	}
};


EnetClient::EnetClient(ENetHost* client)
{
	m_client = client;
	m_peer = NULL;
};

EnetClient::~EnetClient()
{
	enet_host_destroy(m_client);
};




// and finally the HG3DEnet class, which creates servers and clients and initializes networking
//

HG3DEnet::HG3DEnet()
{
	 if (enet_initialize () != 0)
		{
			fprintf (stderr, "An error occurred while initializing ENet.\n");
		}
};

HG3DEnet::~HG3DEnet()
{
	enet_deinitialize();
};

EnetServer* HG3DEnet::createServer(int port)
{
	ENetAddress address;
	ENetHost * server;
	/* Bind the server to the default localhost.     */
	/* A specific host address can be specified by   */
	/* enet_address_set_host (& address, "x.x.x.x"); */
	address.host = ENET_HOST_ANY;
	/* Bind the server to port 1234. */
	address.port = port;
	server = enet_host_create (& address /* the address to bind the server host to */, 
								 32      /* allow up to 32 clients and/or outgoing connections */,
								  3      /* allow up to 2 channels to be used, 0 and 1 */,
								  0      /* assume any amount of incoming bandwidth */,
								  0      /* assume any amount of outgoing bandwidth */);
	if (server == NULL)
	{
		fprintf (stderr, 
				 "An error occurred while trying to create an ENet server host.\n");
		return NULL;
	}
	return (new EnetServer(server));
};

EnetClient* HG3DEnet::createClient()
{
	ENetHost * client;
	client = enet_host_create (NULL /* create a client host */,
				5 /* only allow 5 outgoing connection */,
				3 /* allow up 3 channels to be used, 0 and 1 */,
				0 ,
				0 );
	if (client == NULL)
	{
		fprintf (stderr, 
				 "An error occurred while trying to create an ENet client host.\n");
		return NULL;
	}
	return (new EnetClient(client));
};
	

