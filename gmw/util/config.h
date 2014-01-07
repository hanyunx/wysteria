// config.h
#ifndef __CONFIG_H__BY_SGCHOI
#define __CONFIG_H__BY_SGCHOI

#define NTL_NO_MIN_MAX
#include "../NTL/ZZ.h"
#include "../util/typedefs.h"
using namespace NTL;

#include <vector>

#define TOKEN_PID			"pid"					// player id 
#define TOKEN_NUMINPUT		"num_input"				// #items in server
#define TOKEN_INPUTRANGE	"input_range"			// the range of the input 
#define	TOKEN_INPUT			"input"					// actual input
#define TOKEN_ADDRS			"address-book"			// address book
#define TOKEN_P				"p"						// field
#define TOKEN_G				"g"						// generator
#define TOKEN_SEED			"seed"					// seed
#define TOKEN_NUMPARTIES	"num_parties"			// number of parties
#define TOKEN_LOAD_CIRC		"load-circuit"			// binary circuit file
#define TOKEN_CREATE_CIRC	"create-circuit"			// create a circuit 
#define TOKEN_COMMENT		'%'						// comment

/* Aseem:config file entries for output file, shares input file, shares output file */
#define TOKEN_OUTPUT            "output"
#define TOKEN_SHINPUT           "shinput"
#define TOKEN_SHOUTPUT          "shoutput"
//#define TOKEN_PERFOUTPUT        "perfoutput"

//Aseem: OT Optimization stuff
/*#define TOKEN_DO_OT             "do_ot"
#define TOKEN_DO_OT_NUM         "do_ot_num"
#define TOKEN_OT_FILE           "ot_file"
#define TOKEN_OT_META_FILE      "ot_meta"
#define TOKEN_LOAD_OT_NUM       "load_ot_num"*/

class CConfig 
{
public:
CConfig(){ GetInstance(this); }
~CConfig(){}

public:
static CConfig* GetInstance(CConfig* p = NULL){ static CConfig* sp; if(p) sp = p; return sp;}
	 
public:
BOOL		Load(const char* filename);
//BOOL		LoadAddressBook(const char* filename);
int			GetNumInputs(){ return m_nNumInputs; }
//int			GetNumParties(){ return m_nNumParties;}
//BOOL		IsServer(){ return m_nPID < m_nNumParties-1; }
//int			GetPID(){ return m_nPID; }
void		GetInput(vector<int>& v){ v = m_vInputs;}
void            GetShInput(vector<char>& v) { v = m_vShInputs; }
//string		GetSeed(){ return m_strSeed;}
/* USHORT		GetPortPID(int i){ return m_vPorts[i];} */
/* string		GetAddrPID(int i){ return m_vAddrs[i];} */
//ZZ&			GetPrime(){ return m_P;}
//ZZ&			GetGenerator(){ return m_G;}
int			GetInputRange(){ return m_nInputRange;}
string		GetCircFileName(){ return m_strCircFileName;}
//string		GetCircCreateName(){ return m_strCircCreateName; }
//vector<int>& GetCircCreateParams(){ return m_vCircCreateParam; }

string       GetOutputFileName() { return m_strOutputFileName; }
string       GetOutputSharesFileName() { return m_strOutputSharesFileName; }
//string       GetPerfFileName() { return m_PerfFileName; }

/* Aseem: OT functions */
/*string       GetOTFileName() { return m_strOTFile; }
string       GetOTMetaFileName() { return m_strOTMetaFile; }
bool         GetDoOT() { return m_doOT; }
int          GetDoOTNum() { return m_doOTNum; }
int          GetLoadOTNum() { return m_loadOTNum; }*/

private:
vector<int>	        m_vInputs;
/* Aseem:input shares vector */
vector<char>            m_vShInputs;
/* vector<USHORT>		m_vPorts; */
/* vector<string>		m_vAddrs; */
//int		        m_nPID;  
//ZZ			m_P;
//ZZ			m_G;
//string			m_strSeed;
//int			m_nNumParties;
int			m_nInputRange;
int			m_nNumInputs;
string			m_strCircFileName;
//string			m_strCircCreateName;
/* Aseem:output file names */
string                  m_strOutputFileName;
string                  m_strOutputSharesFileName;
//string                  m_PerfFileName;
//vector<int>		m_vCircCreateParam;

//Aseem: OT Stuff
/*bool                    m_doOT;
int                     m_doOTNum;
string                  m_strOTFile;
string                  m_strOTMetaFile;
int                     m_loadOTNum;*/
};

#endif //__CONFIG_H__BY_SGCHOI
