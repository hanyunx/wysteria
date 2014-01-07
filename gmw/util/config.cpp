#include "config.h"
#include <fstream>
#include <sstream>
  

BOOL CConfig::Load(const char* filename)
{
    ifstream fs(filename);
    if(!fs.is_open()) return FALSE;

    string sLine;
    for( int nLineNum = 1; fs.good(); nLineNum++ )
	{ 	
	    getline(fs, sLine);
	    if( sLine.empty() ) continue;

	    istringstream is(sLine);

	    string sParam;
	    is >> sParam;
	    if( is.bad() )
		{
		    cout  << nLineNum << ": " << sLine << endl;
		    cout << " error in parsing the parameter" << endl;
		    return FALSE;
		}

	    if( sParam == "" || sParam[0] == TOKEN_COMMENT ) continue;
		 
	    // if( sParam == TOKEN_PID)
	    // 	{
	    // 	    is >> m_nPID; 
	    // 	}
	    if ( sParam == TOKEN_NUMINPUT )
		{	
		    is >> m_nNumInputs;
		    m_vInputs.reserve(m_nNumInputs);
		}
	    else if ( sParam == TOKEN_INPUT )
		{
		    string fname;
		    is >> fname;
		    ifstream fs(fname.c_str());
		    int in;
		    if( fs.is_open() )
			{
			    for( fs >> in; fs.good(); fs >> in )
				m_vInputs.push_back(in); 
			}
		    fs.close();
		}
	    /* Aseem:read input shares */
	    else if ( sParam == TOKEN_SHINPUT )
		{
		    string fname;
		    is >> fname;
		    ifstream fs(fname.c_str());
		    char in;
		    if( fs.is_open() )
			{
			    for( fs >> in; fs.good(); fs >> in )
				m_vShInputs.push_back(in); 
			}
		    fs.close();
		}
	    else if (sParam == TOKEN_OUTPUT)
		{
		    is >> m_strOutputFileName;
		}
	    // else if (sParam == TOKEN_DO_OT)
	    // 	{
	    // 	    string ot;
	    // 	    is >> ot;
	    // 	    if(ot == "y") {
	    // 		m_doOT = true;
	    // 	    } else {
	    // 		m_doOT = false;
	    // 	    }
	    // 	}
	    // else if (sParam == TOKEN_DO_OT_NUM)
	    // 	{
	    // 	    is >> m_doOTNum;
	    // 	}
	    // else if (sParam == TOKEN_OT_FILE)
	    // 	{
	    // 	    is >> m_strOTFile;
	    // 	}
	    // else if (sParam == TOKEN_OT_META_FILE)
	    // 	{
	    // 	    is >> m_strOTMetaFile;
	    // 	}
	    // else if (sParam == TOKEN_LOAD_OT_NUM)
	    // 	{
	    // 	    is >> m_loadOTNum;		    
	    // 	}
	    else if (sParam == TOKEN_SHOUTPUT)
		{
		    is >> m_strOutputSharesFileName;
		}
	    // else if (sParam == TOKEN_PERFOUTPUT)
	    // 	{
	    // 	    is >> m_PerfFileName;
	    // 	}
	    // else if ( sParam == TOKEN_ADDRS )
	    // 	{
	    // 	    string fname;
	    // 	    is >> fname;
	    // 	    LoadAddressBook(fname.c_str());
	    // 	}
	    // else if ( sParam == TOKEN_P	)
	    // 	{
	    // 	    is >> m_P;
	    // 	}
	    // else if ( sParam == TOKEN_G )
	    // 	{
	    // 	    is >> m_G;
	    // 	}
	    // else if ( sParam == TOKEN_SEED )
	    // 	{
	    // 	    is >> m_strSeed;
	    // 	}
	    // else if ( sParam == TOKEN_NUMPARTIES )
	    // 	{
	    // 	    is >> m_nNumParties;
	    // 	}
	    else if ( sParam == TOKEN_LOAD_CIRC )
		{
		    is >> m_strCircFileName;
		}
	    // else if ( sParam == TOKEN_CREATE_CIRC )
	    // 	{
	    // 	    is >> m_strCircCreateName;
	    // 	    int v;
	    // 	    while( is.good() )
	    // 		{
	    // 		    is >> v;
	    // 		    m_vCircCreateParam.push_back(v);
	    // 		}
	    // 	}		
	    else 
		{
		    cout << "unrecognized command in config line (skip)" << nLineNum << ": " << sLine << endl;
		}
	
	    if( is.bad())
		{
		    cout << "error in line " << nLineNum << ": " << sLine << endl;
		    return FALSE;
		} 
	}
    return true;
}

// BOOL CConfig::LoadAddressBook(const char* filename)
// {
//     ifstream fs(filename);
//     if(!fs.is_open()) return FALSE;

//     m_vAddrs.resize(m_nNumParties);
//     m_vPorts.resize(m_nNumParties);

//     string sLine;
//     for( int nLineNum = 1; fs.good(); nLineNum++ )
// 	{ 	
// 	    getline(fs, sLine);
// 	    if( sLine.empty() || sLine[0] == TOKEN_COMMENT ) continue;

// 	    istringstream is(sLine);
		
// 	    int    nPID;  
// 	    string sAddr;
// 	    USHORT nPort;

// 	    is >> nPID >> sAddr >> nPort; 
// 	    if( is.bad() )
// 		{
// 		    cout  << nLineNum << ": " << sLine << endl;
// 		    cout << " error in parsing the address book" << endl;
// 		    return FALSE;
// 		}

// 	    if( nPID < m_nNumParties )
// 		{
// 		    m_vAddrs[nPID] = sAddr;
// 		    m_vPorts[nPID] = nPort;
// 		}
// 	}

//     return TRUE;
// }
		
