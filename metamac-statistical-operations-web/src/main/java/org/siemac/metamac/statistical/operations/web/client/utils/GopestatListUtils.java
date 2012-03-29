package org.siemac.metamac.gopestat.web.client.utils;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import org.siemac.metamac.gopestat.core.dto.serviceapi.CollMethodDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.CostDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceTypeDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OfficialityTypeDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.SurveySourceDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.SurveyTypeDto;

public class GopestatListUtils {

    /**
     * Returns {@link LinkedHashMap} of {@link SurveyTypeDto}
     * 
     * @return
     */
    public static LinkedHashMap<String, String> getSurveyTypeHashMap(List<SurveyTypeDto> list) {
        LinkedHashMap<String, String> hashMap = new LinkedHashMap<String, String>();
        for (SurveyTypeDto type : list) {
            hashMap.put(type.getId().toString(), type.getIdentifier());
        }
        return hashMap;
    }

    /**
     * Returns {@link LinkedHashMap} of {@link InstanceTypeDto}
     * 
     * @return
     */
    public static LinkedHashMap<String, String> getInstanceTypeHashMap(List<InstanceTypeDto> list) {
        LinkedHashMap<String, String> hashMap = new LinkedHashMap<String, String>();
        for (InstanceTypeDto type : list) {
            hashMap.put(type.getId().toString(), type.getIdentifier());
        }
        return hashMap;
    }

    /**
     * Returns {@link LinkedHashMap} of {@link OfficialityTypeDto}
     * 
     * @return
     */
    public static LinkedHashMap<String, String> getOfficialityTypeHashMap(List<OfficialityTypeDto> list) {
        LinkedHashMap<String, String> hashMap = new LinkedHashMap<String, String>();
        for (OfficialityTypeDto type : list) {
            hashMap.put(type.getId().toString(), type.getIdentifier());
        }
        return hashMap;
    }

    /**
     * Returns {@link LinkedHashMap} of {@link SurveySourceDto}
     * 
     * @return
     */
    public static LinkedHashMap<String, String> getSurveySourceHashMap(List<SurveySourceDto> list) {
        LinkedHashMap<String, String> hashMap = new LinkedHashMap<String, String>();
        for (SurveySourceDto type : list) {
            hashMap.put(type.getId().toString(), type.getIdentifier());
        }
        return hashMap;
    }

    /**
     * Returns {@link LinkedHashMap} of {@link CollMethodDto}
     * 
     * @return
     */
    public static LinkedHashMap<String, String> getCollMethodsHashMap(List<CollMethodDto> list) {
        LinkedHashMap<String, String> hashMap = new LinkedHashMap<String, String>();
        for (CollMethodDto type : list) {
            hashMap.put(type.getId().toString(), type.getIdentifier());
        }
        return hashMap;
    }

    /**
     * Returns {@link LinkedHashMap} of {@link CostDto}
     * 
     * @return
     */
    public static LinkedHashMap<String, String> getCostHashMap(List<CostDto> list) {
        LinkedHashMap<String, String> hashMap = new LinkedHashMap<String, String>();
        for (CostDto type : list) {
            hashMap.put(type.getId().toString(), type.getIdentifier());
        }
        return hashMap;
    }

    /**
     * Returns {@link SurveyTypeDto} from id
     * 
     * @param officialityTypeDtos
     * @param id
     * @return
     */
    public static SurveyTypeDto getSurveyTypeDto(String id, List<SurveyTypeDto> list) {
        if (id != null && !id.isEmpty()) {
            Long idType = Long.valueOf(id);
            for (SurveyTypeDto o : list) {
                if (o.getId().compareTo(idType) == 0) {
                    return o;
                }
            }
        }
        return null;
    }

    /**
     * Returns {@link InstanceTypeDto} from id
     * 
     * @param officialityTypeDtos
     * @param id
     * @return
     */
    public static InstanceTypeDto getInstanceTypeDto(String id, List<InstanceTypeDto> list) {
        if (id != null && !id.isEmpty()) {
            Long idType = Long.valueOf(id);
            for (InstanceTypeDto o : list) {
                if (o.getId().compareTo(idType) == 0) {
                    return o;
                }
            }
        }
        return null;
    }

    /**
     * Returns {@link SurveySourceDto} from id
     * 
     * @param officialityTypeDtos
     * @param id
     * @return
     */
    public static SurveySourceDto getSurveySourceDto(String id, List<SurveySourceDto> list) {
        if (id != null && !id.isEmpty()) {
            Long idType = Long.valueOf(id);
            for (SurveySourceDto o : list) {
                if (o.getId().compareTo(idType) == 0) {
                    return o;
                }
            }
        }
        return null;
    }

    /**
     * Returns {@link OfficialityTypeDto} from id
     * 
     * @param officialityTypeDtos
     * @param id
     * @return
     */
    public static OfficialityTypeDto getOfficialityTypeDto(String id, List<OfficialityTypeDto> list) {
        if (id != null && !id.isEmpty()) {
            Long idOfficialityType = Long.valueOf(id);
            for (OfficialityTypeDto o : list) {
                if (o.getId().compareTo(idOfficialityType) == 0) {
                    return o;
                }
            }
        }
        return null;
    }

    /**
     * Returns {@link CollMethodDto} from id
     * 
     * @param officialityTypeDtos
     * @param id
     * @return
     */
    public static CollMethodDto getCollMethodDto(String id, List<CollMethodDto> list) {
        if (id != null && !id.isEmpty()) {
            Long idType = Long.valueOf(id);
            for (CollMethodDto o : list) {
                if (o.getId().compareTo(idType) == 0) {
                    return o;
                }
            }
        }
        return null;
    }

    /**
     * Returns {@link CostDto} from id
     * 
     * @param officialityTypeDtos
     * @param id
     * @return
     */
    public static CostDto getCostDto(String id, List<CostDto> list) {
        if (id != null && !id.isEmpty()) {
            Long idType = Long.valueOf(id);
            for (CostDto o : list) {
                if (o.getId().compareTo(idType) == 0) {
                    return o;
                }
            }
        }
        return null;
    }

    /**
     * Returns a list of {@link CostDto} from list of identifiers
     * 
     * @param officialityTypeDtos
     * @param id
     * @return
     */
    public static List<CostDto> getCostDtos(String[] ids, List<CostDto> list) {
        List<CostDto> costDtos = new ArrayList<CostDto>();
        for (String id : ids) {
            CostDto costDto = getCostDto(id, list);
            if (costDto != null) {
                costDtos.add(costDto);
            }
        }
        return costDtos;
    }

    /**
     * Returns {@link String} with list of identifiers from a list of {@link CostDto}
     * 
     * @param list
     * @return
     */
    public static String getCostDtoListToString(Set<CostDto> set) {
        List<CostDto> list = new ArrayList<CostDto>(set);
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < list.size(); i++) {
            builder.append(i != 0 ? ",  " : "");
            builder.append(list.get(i).getIdentifier());
        }
        return builder.toString();
    }

}
