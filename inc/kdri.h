
#ifndef KDRI_INC_GUARD
#define KDRI_INC_GUARD

#include <stdint.h>

extern const uint8_t* hello_rust();

struct KettlerDevice {
  uint8_t* name; // null terminated string
  uint8_t addr[6]; // MAC address of device
};

typedef void* KettlerConnection;

/* Start bluetooth inquiry and find all Kettler devices in range. "timeout" is in 1/8 seconds. This function will block for "1/8*timeout" seconds. */
extern int32_t kdri_scan_devices(struct KettlerDevice* device_array, uint32_t max_devices, uint32_t timeout);
extern void kdri_free_device(struct KettlerDevice* device_array);

/* Name gets freed with "kdri_free_device". */
extern void kdri_get_device_addr_string(struct KettlerDevice* device, uint8_t* name_ptr);

/* Connect to a specific kettler device */
extern KettlerConnection kdri_connect(struct KettlerDevice* device);
extern void kdri_free_connection(KettlerConnection connection);

extern int32_t kdri_get_speed(KettlerConnection connection);
extern int32_t kdri_get_incline(KettlerConnection connection);
extern int32_t kdri_get_online(KettlerConnection connection);

extern void kdri_set_speed(KettlerConnection connection, int32_t v);
extern void kdri_set_incline(KettlerConnection connection, int32_t v);
extern void kdri_set_online(KettlerConnection connection, int32_t v);

#endif // KDRI_INC_GUARD
