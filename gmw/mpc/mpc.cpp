// main.cpp
#include "../util/typedefs.h"
#include "../util/config.h"
#include "../circuit/circuit.h"

#include "party.h"

#include<ctime>
#include<sys/time.h>
#include<sstream>
#include<fstream>

using namespace std;

void PrintOutput(const vector<int>& vOutput, ofstream& outstream)
{
    //cout << "output:" << endl;
    //cout << "(binary)";
    for( UINT i=0; i<vOutput.size(); i++ ) {
	//cout << " " << (int) vOutput[i];
	outstream << " " << (int) vOutput[i];
    }
    //cout << endl;

    ZZ out;
    out.SetSize(vOutput.size());
	
    //cout << "(numeric:big-endian) ";

    int size = vOutput.size();
    for( int i=0; i<size; i++ )
	{
	    if( bit(out,i) != vOutput[i] )
		SwitchBit(out,i);
	}
    //cout << out << endl;


    //cout << "(numeric:little-endian) ";

    for( int i=0; i<size; i++ )
	{
	    if( bit(out,i) != vOutput[size-i-1] )
		SwitchBit(out,i);
	}
    //cout << out << endl;
}

void PrintShares(const vector<char>& shares, ofstream& os)
{
    for(vector<char>::const_iterator it = shares.begin(); it != shares.end();
	++it) {
	os << *it;
    }
}

int Run(const char *config_file_name, CParty* pParty, struct perf_data &perf)
{
    struct timeval initial;
    //struct timeval configloaded;
    //struct timeval partydone;
    struct timeval final;

    gettimeofday(&initial, NULL);
    CConfig* pConfig = new CConfig();
    if(!pConfig->Load(config_file_name)) {	 
	cout << "Failure in opening the config file: " << config_file_name << endl;
	return 0;
    }
    //gettimeofday(&configloaded, NULL);
	
    int c = pParty->Run(perf);

    //gettimeofday(&partydone, NULL);
	
    ofstream outfile;
    outfile.open(pConfig->GetOutputFileName().c_str(), ios::out | ios::trunc);
    PrintOutput( pParty->GetOutput(), outfile );	
    outfile.close();
	
    ofstream outfile1;
    outfile1.open(pConfig->GetOutputSharesFileName().c_str(), ios::out | ios::trunc);
    vector<char> shares;
    pParty->GetSharesOutput(shares);
    PrintShares(shares, outfile1);
    outfile1.close();

    gettimeofday(&final, NULL);
    long int initialu = initial.tv_sec * 1000000 + initial.tv_usec;
    //long int configloadedu = configloaded.tv_sec * 1000000 + configloaded.tv_usec;
    //long int partydoneu = partydone.tv_sec * 1000000 + partydone.tv_usec;
    long int finalu = final.tv_sec * 1000000 + final.tv_usec;

    perf.total = finalu - initialu;
    //cout << "total time to load config: " << configloadedu - initialu << endl;
    //cout << "total time to run party: " << partydoneu - configloadedu << endl;

    //cout << "total time spent: " << finalu - initialu << endl;

    // ofstream outfile2;
    // outfile2.open(pConfig->GetPerfFileName().c_str(), ios::out | ios::trunc);

    // outfile2 << perf.load << " "
    // 	     << perf.input_setup << " "
    // 	     << perf.compute_gates << " " << perf.output_setup << " "
    // 	     << finalu - initialu;
	
    // outfile2.close ();
 	 
    //double tt1 = clock();
    //cout << endl << "elapsed " <<  (tt1-tt)/CLOCKS_PER_SEC << " seconds." << endl;
    return c;
}

BOOL LoadAddressBook(const char* filename, int num_parties,
		     vector<string> &addrs,
		     vector<USHORT> &ports)
{
    ifstream fs(filename);
    if(!fs.is_open()) return FALSE;
        
    addrs.resize(num_parties);
    ports.resize(num_parties);

    string sLine;
    for( int nLineNum = 1; fs.good(); nLineNum++ )
	{ 	
	    getline(fs, sLine);
	    if( sLine.empty() || sLine[0] == '%') continue;

	    istringstream is(sLine);
		
	    int    nPID;  
	    string sAddr;
	    USHORT nPort;

	    is >> nPID >> sAddr >> nPort; 
	    if( is.bad() )
		{
		    cout  << nLineNum << ": " << sLine << endl;
		    cout << " error in parsing the address book" << endl;
		    return FALSE;
		}

	    if( nPID < num_parties )
		{
		    addrs[nPID] = sAddr;
		    ports[nPID] = nPort;
		}
	}

    return TRUE;
}

int main(int argc, char **argv)
{
    //open a socket and listen on it
    
    //if(argc < 7) {
    if(argc != 6) {
	//cout << "usage: mpc.exe <server_port> <num_parties> <address_file> <self_id> <num_and_gates> <if wysteria 1, else 0> <if wm = 0 then config file>" << endl;
	cout << "usage: mpc.exe <server_port> <num_parties> <address_file> <self_id> <num_and_gates>" << endl;
	exit(1);
    }
    int server_port = atoi(argv[1]);
    
    int num_parties = atoi(argv[2]);
    const char *addr_file = argv[3];

    int self_id = atoi(argv[4]);

    int num_and_gates = atoi(argv[5]);

    //int wm = atoi(argv[6]);
    int wm = 1;

    vector<string> addrs;
    vector<USHORT> ports;
    
    LoadAddressBook(addr_file, num_parties, addrs, ports);

    CParty* pParty = new CParty();
    
    pParty->EstablishConnection(addrs, ports, num_parties, self_id, num_and_gates);
        
    struct perf_data all_runs;
    
    if(wm) {
	CSocket main_sock, accept_sock;
    
	//take this from command line/config file ?
	string ip = "localhost";

	int config_file_name_length = 12;
	char config_file_name[config_file_name_length];
    
	BOOL accept_ok;
    
	if(!main_sock.Socket()) {
	    cout << "Main server socket creation error" << endl;
	    goto ret;
	}

	if(!main_sock.Bind(server_port, ip)) {
	    cout << "Main server socket bind error" << endl;
	    goto ret;
	}

	if(!main_sock.Listen()) {
	    cout << "Main server socket listen error" << endl;
	    goto ret;
	}
    
	//we don't expect to serve more than one client
	cout << "Server ready" << endl;
	accept_ok = main_sock.Accept(accept_sock);
	if(!accept_ok) {
	    cout << "Main server socket accept error" << endl;
	    goto ret;
	}
	while(true) {
	    //fill null in the local buffer
	    memset(config_file_name, 0, config_file_name_length);
	
	    int receive_length = accept_sock.Receive(config_file_name,
						     config_file_name_length - 1);
	    if(receive_length != (config_file_name_length -1)) {
	        //cout << "Main server config file name more than " << config_file_name_length << " characters" << endl;
		goto ret;
	    }
	    struct perf_data this_run;
	    int c = Run(config_file_name, pParty, this_run);
	    /*{
		cout << "Load time: " << this_run.load << endl;
		cout << "Input setup time: " << this_run.input_setup << endl;
		cout << "Compute gates time: " << this_run.compute_gates << endl;
		cout << "Output setup time: " << this_run.output_setup << endl;
		cout << "Total time: " << this_run.total << endl;
		}*/
	    {
		all_runs.load += this_run.load;
		all_runs.input_setup += this_run.input_setup;
		all_runs.compute_gates += this_run.compute_gates;
		all_runs.output_setup += this_run.output_setup;
		all_runs.total += this_run.total;
	    }
	    int send_ok = accept_sock.Send(&c, sizeof(c));
	    if(send_ok != sizeof(c)) {
		cout << "Main server result send failure" << endl;
		goto ret;
	    }
	}
    } else {
	    if(argc != 8) {
		cout << "Enter a config file name for non-wm" << endl;
		goto ret;
	    }
	    int c = Run(argv[7], pParty, all_runs);
	    goto ret;
    }

 ret:
    /*cout << "Total time spent in loading circuit files: " << all_runs.load << endl;
    cout << "Total time spent in input setup: " << all_runs.input_setup << endl;
    cout << "Total time spent in computing gates: " << all_runs.compute_gates << endl;
    cout << "Total time spent in output setup: " << all_runs.output_setup << endl;
    cout << "Total time: " << all_runs.total << endl;*/
    delete pParty;
    return 0;
}

// int main(int argc, char** argv)
// {
// 	if( argc != 2 )
// 	{
// 		cout << "usage: mpc.exe config_file " << endl;
// 		return 0;
// 	}

// 	double tt = clock();

// 	CConfig* pConfig = new CConfig();
// 	if(!pConfig->Load(argv[1]))
// 	{
// 		cout << "failure in opening the config file: " << argv[1] << endl;
// 		return 0;
// 	}

// 	CParty* pParty = new CParty();
// 	struct timeval initial;
// 	struct timeval final;

// 	struct perf_data perf;
	
// 	gettimeofday(&initial, NULL);
// 	int c = pParty->Run(perf);
// 	gettimeofday(&final, NULL);


// 	long int initialu = initial.tv_sec * 1000000 + initial.tv_usec;
// 	long int finalu = final.tv_sec * 1000000 + final.tv_usec;

// 	cout << "total time spent: " << finalu - initialu << endl;
// 	perf.total = finalu - initialu;
	
// 	ofstream outfile;
// 	outfile.open(pConfig->GetOutputFileName().c_str(), ios::out | ios::trunc);

// 	PrintOutput( pParty->GetOutput(), outfile );
	
// 	outfile.close();
	
// 	ofstream outfile1;
// 	outfile1.open(pConfig->GetOutputSharesFileName().c_str(), ios::out | ios::trunc);

// 	vector<char> shares;
// 	pParty->GetSharesOutput(shares);
// 	PrintShares(shares, outfile1);

// 	outfile1.close();

// 	ofstream outfile2;
// 	outfile2.open(pConfig->GetPerfFileName().c_str(), ios::out | ios::trunc);

// 	outfile2 << perf.load << " " << perf.connect << " "
// 		 << perf.ot_setup << " "
// 		 << perf.input_setup << " "
// 		 << perf.compute_gates << " " << perf.output_setup << " "
// 		 << perf.total;
	
// 	outfile2.close ();

// 	delete pParty;
 	 
// 	double tt1 = clock();
// 	//cout << endl << "elapsed " <<  (tt1-tt)/CLOCKS_PER_SEC << " seconds." << endl;
// 	return c;
// }
