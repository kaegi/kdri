#include <stdlib.h>
#include <sys/socket.h>
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/hci_lib.h>

// sdp service discovery
#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>

#include <kdri.h>

#define LENGTH(x) (int32_t)(sizeof(x) / sizeof(x[0]))

uint8_t prefix(const char *str, const char *pre) {
    return strncmp(pre, str, strlen(pre)) == 0;
}

int32_t c__kdri_scan_devices(struct KettlerDevice* device_array, uint32_t max_devices, uint32_t timeout) {
  // illegal arguments
	if(max_devices <= 0 || device_array == NULL) return -1;

	inquiry_info* ii = NULL;
	int num_rsp = 0, max_rsp = 256;
	int dev_id, socket, flags;

	char addr[19] = { 0 };
	char name[256] = { 0 };

	// get socket to bluetooth microcontoller
	dev_id = hci_get_route(NULL);
	socket = hci_open_dev(dev_id);
	if(dev_id < 0 || socket < 0) {
		perror("opening bluetooth socket");
		exit(1);
	}

	max_rsp = 255; // maximum number of devices that can be found
	flags = IREQ_CACHE_FLUSH;
	ii = (inquiry_info*)malloc(max_rsp * sizeof(inquiry_info));

	// find all bluetooth device_array in range
	num_rsp = hci_inquiry(dev_id, timeout, max_rsp, NULL, &ii, flags);
	if(num_rsp < 0) {
		perror("hci_inquiry");
		return -1;
	}

	// -------------------------------------------
	// find devices from kettler

	// series names taken from http://de.sport.kettler.net/fileadmin/documents/kataloge-pdf/Sport_EV_2015_en.pdf
	const char* kettlerDevicePrefixes[] = {"TOUR", "RACER", "ERGO", "RECUMBENT", "UNIX", "SKYLON", "RUN", "TRACK"};
	const int32_t numKettlerDevicePrefixes = LENGTH(kettlerDevicePrefixes);

	int32_t numKettlerDevicesFound = 0;

	for(int32_t i = 0; i < num_rsp; i++) {
		ba2str(&ii[i].bdaddr, addr);
		memset(name, 0, sizeof(name));
		if(hci_read_remote_name(socket, &ii[i].bdaddr, sizeof(name), name, 0) < 0)
			continue;

		// does device have right prefix?
		uint8_t isFromKettler = 0;
		for(int k = 0; k < numKettlerDevicePrefixes; k++) {
			if(!prefix(name, kettlerDevicePrefixes[k]))
				continue;
			// this device is probably from Kettler
			isFromKettler = 1;
			break;
		}

		if(isFromKettler) {
			// insert device name and address into output list
			int nameLength = strlen(name) + 1;
			memcpy(device_array[numKettlerDevicesFound].addr, ii[i].bdaddr.b, sizeof(ii[0].bdaddr.b));
			device_array[numKettlerDevicesFound].name = malloc(sizeof(uint8_t) * nameLength);
			memcpy(device_array[numKettlerDevicesFound].name, name, nameLength);

			numKettlerDevicesFound++;
			if(numKettlerDevicesFound == max_devices)
				break;
		}
	}

	free(ii);
	close(socket);

	return numKettlerDevicesFound;
}

void c__kdri_free_device(struct KettlerDevice* device) {
  free(device->name);
  memset(device, 0, sizeof(struct KettlerDevice));
}


/*

   Returns the channel that is associated with this rfcomm service with given UUID.

   Code from: https://people.csail.mit.edu/albert/bluez-intro/x604.html

   Returns -1 if connection failed or the service did not specify a rfcomm
   channel.
*/
int32_t c__get_rfcomm_channel(bdaddr_t addr, uint16_t class16) {
	int32_t channel = -1;


	// connect to SDP server on remote machine
	sdp_session_t* session = sdp_connect(BDADDR_ANY, &addr, SDP_RETRY_IF_BUSY);
	if(session == NULL) {
		printf("E: bluetooh device not found for service discovery (SDP)");
		return -1;
	}

	// specify uuid
	uuid_t service_uuid;
	sdp_uuid16_create(&service_uuid, class16);
	sdp_list_t* search_list = sdp_list_append(NULL, &service_uuid);

    // specify that we want a list of all the matching applications' attributes
    uint32_t range = 0x0000ffff;
    sdp_list_t* attrid_list = sdp_list_append( NULL, &range );

    // get a list of service records that have UUID 0xabcd
	sdp_list_t* response_list = NULL;
    int err = sdp_service_search_attr_req( session, search_list, SDP_ATTR_REQ_RANGE, attrid_list, &response_list);
	if(err < 0) {
		printf("E: failed to fetch service records from remote device");
		return -1;
	}

	// --------------------------------------------------
	// parse service

	sdp_list_t *r = response_list;

	// go through each of the service records
	for (; r; r = r->next ) {
		sdp_record_t *rec = (sdp_record_t*) r->data;
		sdp_list_t *proto_list;

		// get a list of the protocol sequences
		if( sdp_get_access_protos( rec, &proto_list ) == 0 ) {
			sdp_list_t *p = proto_list;

			// go through each protocol sequence
			for( ; p ; p = p->next ) {
				sdp_list_t *pds = (sdp_list_t*)p->data;

				// go through each protocol list of the protocol sequence
				for( ; pds ; pds = pds->next ) {

					// check the protocol attributes
					sdp_data_t *d = (sdp_data_t*)pds->data;
					int proto = 0;
					for( ; d; d = d->next ) {
						switch( d->dtd ) {
							case SDP_UUID16:
							case SDP_UUID32:
							case SDP_UUID128:
								proto = sdp_uuid_to_proto( &d->val.uuid );
								break;
							case SDP_UINT8:
								if( proto == RFCOMM_UUID ) { channel = d->val.int8; }
								break;
						}
					}
				}
				sdp_list_free( (sdp_list_t*)p->data, 0 );
			}
			sdp_list_free( proto_list, 0 );
		}
		sdp_record_free( rec );
	}

	sdp_close(session);

	return channel;
}
